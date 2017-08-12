#!/usr/bin/env python3


"""Low-level example of how to spend a standard pay-to-pubkey-hash (P2PKH) txout"""

import sys
if sys.version_info.major < 3:
    sys.stderr.write('Sorry, Python 3.x required by this example.\n')
    sys.exit(1)

import hashlib

from bitcoincashlib.bitcoin import SelectParams
from bitcoincashlib.bitcoin.core import b2x, x, lx, COIN, COutPoint, CTxOut, CMutableTxOut, CMutableTxIn, CTransaction, CMutableTransaction, Hash160, Hash
from bitcoincashlib.bitcoin.core.script import CScript, OP_DUP, OP_HASH160, OP_EQUALVERIFY, OP_CHECKSIG, SignatureHash, SIGHASH_ALL, SIGHASH_FORKID
from bitcoincashlib.bitcoin.core.scripteval import VerifyScript, SCRIPT_VERIFY_P2SH, VerifyOpFailedError
from bitcoincashlib.bitcoin.wallet import CBitcoinAddress, CBitcoinSecret
#Args
from optparse import OptionParser
import sys


SelectParams('mainnet')

parser = OptionParser()
parser.add_option( "-t", "--tx", dest="tx_file"
                 , help="TxPrevOut list")
(options, args) = parser.parse_args()

if not options.tx_file:
  print("Please specify <tx_file>")
  exit(1)

tx_file = open(options.tx_file, 'r')

txsToPush = []

for line in tx_file:
  cleanLine = line.lstrip("SIGNTX").strip('\n')
  txData = cleanLine.split('x')[0]
  prevOut = CTxOut.deserialize( x(cleanLine.split('x')[1]) )
  txin_scriptPubKey = prevOut.scriptPubKey
  if not txData:
    print('Please specify tx to sign')
    exit(1) 

  txi = CTransaction.deserialize( x(txData) )
  tx = CMutableTransaction.from_tx(txi)

  destAddr = CBitcoinAddress.from_scriptPubKey(txin_scriptPubKey)
  print("Enter WIF private key (starts with '5', 'K', or 'L') for address " + str(destAddr) + "   (spaces are removed)") 
  seckeyWifRaw = sys.stdin.readline(100).strip('\n')
  seckeyWif = ''.join(seckeyWifRaw.split(' '))    # remove spaces 
  seckey = CBitcoinSecret(seckeyWif)

  hashflag = SIGHASH_ALL | SIGHASH_FORKID
  sighash = SignatureHash(txin_scriptPubKey, tx, 0, hashflag, prevOut.nValue)
  print( "Signing prevOut of value: " + str(prevOut.nValue) + " (sighash " + str(hashflag) + ")" )
  sig = seckey.sign(sighash) + bytes([hashflag])

  # Set the scriptSig of our transaction input appropriately.
  print("Signed input with pubkey " + b2x(seckey.pub))
  print( str(seckey.is_compressed) )
  tx.vin[0].scriptSig = CScript([sig, seckey.pub])

  # Verify the signature worked.
  try:
    VerifyScript(tx.vin[0].scriptSig, txin_scriptPubKey, tx, 0, prevOut.nValue, (SCRIPT_VERIFY_P2SH,))
  except VerifyOpFailedError as err:
    if err.sop == OP_EQUALVERIFY:
      print("ERROR: Entered private key does not match address " + str(destAddr))
      exit(1)
  else:
    txsToPush.append( b2x(tx.serialize()) )


tx_file.close()

print("Writing signed transactions to " + str(options.tx_file) + "...")
fout = open(options.tx_file, 'a')
for txHex in txsToPush:
  fout.write( "PUSHTX" + txHex + "\n" )
fout.close()

print("Done!")

