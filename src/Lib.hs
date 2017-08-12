{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Aeson
import Data.Aeson.Types
import Network.Bitcoin.AddrIndex.API
import Data.Monoid
import Data.Maybe
import Data.Either
import Data.String.Conversions
import Data.Proxy
import Servant.Client
import Control.Monad
import Network.HTTP.Client.TLS
import Network.HTTP.Client      hiding (Proxy)
import System.IO
import Control.Retry
import Options.Generic
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import qualified Network.Haskoin.Transaction          as HT
import qualified Network.Haskoin.Crypto               as HC
import qualified Network.Haskoin.Script as HS
import qualified Data.ByteString.Base16                         as B16
import qualified Data.Serialize                 as Bin



defaultFee :: Word
defaultFee = 100    -- Satoshis per byte
feeOrDefault :: Maybe Int -> Word
feeOrDefault = maybe defaultFee fromIntegral

defaultTipPercent :: Double
defaultTipPercent = 1.0
tipOrDefault :: Maybe Double -> Double
tipOrDefault = fromMaybe defaultTipPercent

tipAddress :: HC.Address
tipAddress = "1BBDuJvdTajnRsYDqj7gZ4ExPJFK8f5uNn"

serverUrl :: BaseUrl
serverUrl = BaseUrl Https "blockchain.runeks.me" 443 ""

newtype FileName = FileName { fileName :: String }
    deriving (Eq, Show, Read)

newtype Address = Address String
    deriving (Eq, Show, Read)

toAddress :: Address -> HC.Address
toAddress (Address str) =
    either parseOrFail id $ eitherDecode ("\"" <> cs str <> "\"")
  where
    parseOrFail = error $ unwords [ "Failed to parse Bitcoin address from", str ]

data AppConf w =
    Create
    { unsigfile :: w ::: FileName     <?> "Transaction file to which unsigned transactions will be written (afterwards, move to offline device for signing)"
    , dest      :: w ::: Address      <?> "Send redeemed funds to this address"
    , redeem    :: w ::: [Address]    <?> "Redeem outputs paying to this/these address(es) (if absent: read addresses from stdin line-by-line)"
    , txfee     :: w ::: Maybe Int    <?> "Transaction fee in satoshis per byte             (default: 100)"
    , tip       :: w ::: Maybe Double <?> "Tip (in percent) to developer (0.0 to disable)   (default: 1.0)"
    }
  | Publish
    { txfile    :: w ::: FileName             <?> "Transaction file from which signed transactions will be published (comes from offline device used for signing)"
    } deriving Generic


instance ParseRecord FileName where
    parseRecord = FileName <$> parseRecord
instance ParseField FileName where
    parseField a b = FileName <$> parseField a b
instance ParseFields FileName where
    parseFields a b = FileName <$> parseFields a b

instance ParseRecord Address where
    parseRecord = Address <$> parseRecord
instance ParseField Address where
    parseField a b = Address <$> parseField a b
instance ParseFields Address where
    parseFields a b = Address <$> parseFields a b

instance ParseRecord (AppConf Wrapped)
deriving instance Show (AppConf Unwrapped)

main :: IO ()
main = do
    man <- newManager tlsManagerSettings
    let env = ClientEnv man serverUrl
    cfg <- unwrapRecord "Bitcoin Cash tool"
    case cfg of
        Create unsigFile destAddr uAddrL txFee tipPct -> do
            addrL <- if not (null uAddrL)
                  then return uAddrL
                  else addrReadStdin
            let redeemL = map toAddress addrL
            resLL <- fetchUnspentOuts env redeemL
            when (null $ concat resLL) $
                putStrLn $ unwords [ "No unspent outputs found for address(es)"
                                   , unwords (map (cs . encode) redeemL) ]
            writeUnsigTxs (tipOrDefault tipPct) (feeOrDefault txFee) (toAddress destAddr) (cs $ fileName unsigFile) resLL
        Publish txFile -> do
            fileStr <- readFile (fileName txFile)
            let resEL = map (eitherDecode . cs) (lines fileStr)
            pushL <- mapM (runWithRetry env . pushTx) (rights resEL)
            forM_ pushL (\h -> putStrLn $ "Published tx " ++ cs (encode h))

addrReadStdin :: IO [Address]
addrReadStdin = do
    stdinStr <- getContents
    return $ map Address (lines stdinStr)

fetchUnspentOuts :: ClientEnv -> [HC.Address] -> IO [[AddressFundingInfo]]
fetchUnspentOuts env addrL = do
    let addrResE = map getAddr addrL
    forM_ (lefts addrResE) (\e -> print $ "ERROR: " ++ show e)
    forM (rights addrResE) (runWithRetry env . fetchOuts)

writeUnsigTxs :: Double -> Word -> HC.Address -> FilePath -> [[AddressFundingInfo]] -> IO ()
writeUnsigTxs tipPct txFee depositAddr fileName afiLL = do
    print (tipPct, txFee)
    withFile fileName WriteMode $ \h ->
        forM_ (concat afiLL) $ \afi -> do
            putStrLn $ unwords [ "Writing tx-to-sign for", cs . encode $ asiDestAddress afi ]
            hPrint h (mkRedeemingTx tipPct txFee depositAddr afi)
    putStrLn "Done."

getAddr :: HC.Address -> Either AddrErr HC.Address
getAddr addr@HC.PubKeyAddress{} = Right addr
getAddr addr@HC.ScriptAddress{} = Left (P2shUnsupported addr)

newtype AddrErr = P2shUnsupported HC.Address

instance Show AddrErr where
    show (P2shUnsupported addr) = unwords [ show addr ++ ":", "P2SH addresses not supported" ]

failOnErr :: Show a => ClientEnv -> Either a t -> t
failOnErr _ (Right res) = res
failOnErr (ClientEnv _ url) (Left e) = error $
    unwords [ "ERROR:"
            , "Failed to fetch info from server:"
            , show e
            , "(server URL:"
            , show url ++ ")"
            ]

runWithRetry :: ClientEnv -> ClientM a -> IO a
runWithRetry env clientM = failOnErr env <$> retrying
    (limitRetriesByDelay 5000000 $ exponentialBackoff 100000)  -- Exp. backoff: 100ms start, fail after 5s
    (\_ resE -> return $ retryRes resE)
    (\_ -> runClientM clientM env)
  where
    retryRes (Left (FailureResponse status _ _)) =
        statusCode status > 499 && statusCode status < 600
    retryRes (Left ConnectionError{}) = True
    retryRes _ = False

fetchOuts :: HC.Address -> ClientM [AddressFundingInfo]
fetchOuts = client (Proxy :: Proxy UnspentOuts) . Addr

pushTx :: HT.Tx -> ClientM HT.TxHash
pushTx =
    fmap getResp . client (Proxy :: Proxy PublishTx) . PushTxReq
  where
    getResp (PushTxResp h) = h

data TxPrevOut = TxPrevOut HT.Tx HT.TxOut
instance Show TxPrevOut where
    show (TxPrevOut tx out) = cs $ "SIGNTX" <> cs (BL.init $ BL.tail $ encode tx) <> "x" <> B16.encode (Bin.encode out)

newtype PubTx = PubTx HT.Tx
instance FromJSON PubTx where
    parseJSON = withText "PubTx" (pubTxString . cs)

pubTxString :: String -> Parser PubTx
pubTxString str
  | length str > 10 =
      if take 5 str == "PUBTX"
          then PubTx <$> parseJSON (String $ cs $ drop 5 str)
          else fail $ "invalid PubTx: " ++ str
  | otherwise = fail $ "invalid PubTx: " ++ str

mkRedeemingTx :: Double -> Word -> HC.Address -> AddressFundingInfo -> TxPrevOut
mkRedeemingTx tipPct txFee addr AddressFundingInfo{..} =
    TxPrevOut theTx (mkOut (fromIntegral asiValue) asiDestAddress)
  where
    txSize = 174 + length (HT.txOut theTx) * 34
    txAbsFee = fromIntegral txSize * txFee
    theTx = HT.createTx 1
        [ HT.TxIn (HT.OutPoint asiFundingTxId asiFundingVout) BS.empty maxBound ]
        [ mkOut redeemVal addr ]
        0
    redeemVal = fromIntegral asiValue - fromIntegral txAbsFee - maybe 0 HT.outValue tipOutM
    mkOut val adr = HT.TxOut val (HS.encodeOutputBS $ HS.PayPKHash adr)
    tipOutM = if tipValue > 10000000 then Just $ mkOut tipValue tipAddress else Nothing
    tipValue = round $ tipPct / 100 * realToFrac asiValue
