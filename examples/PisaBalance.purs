-- | This module demonstrates the use Pisa balancer
module Ctl.Examples.PisaBalance
  ( main
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, decodeAeson, parseJsonStringToAeson)
import Cardano.Transaction.Builder (TransactionBuilderStep(..))
import Cardano.Types (AssetName)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.TransactionOutput (TransactionOutput(..))
import Cardano.Types.Value as CV
import Contract.Config
  ( MnemonicSource(..)
  , StakeKeyPresence(..)
  , WalletSpec(..)
  , blockfrostPublicPreprodServerConfig
  , defaultConfirmTxDelay
  , mkBlockfrostBackendParams
  )
import Contract.Config as Contract.Config
import Contract.Log (logError')
import Contract.Monad
  ( Contract
  , liftContractM
  , throwContractError
  )
import Contract.Monad as Contract.Monad
import Contract.Transaction
  ( awaitTxConfirmed
  , buildTx
  , signTransaction
  , submit
  )
import Data.ByteArray (byteArrayFromAscii)
import Internal.BalanceTx.PisaBalanceTx (balanceTxWithPisa)
import Node.Encoding as Encoding
import Node.FS.Aff (readTextFile)

userMnemonicFile :: String
userMnemonicFile =
  "/home/mike/dev/mlabs/pisa-fees-project/configs/atlas-peprod-demo/user-seed"

blockfrostApiKeyFile :: String
blockfrostApiKeyFile =
  "/home/mike/dev/mlabs/pisa-fees-project/configs/atlas-peprod-demo/blockfrost-api-key"

main :: Effect Unit
main =
  Contract.Monad.launchAff_ do
    blockfrostKey <- readTextFile Encoding.UTF8 blockfrostApiKeyFile
    void
      $ Contract.Monad.runContract (contractParams blockfrostKey)
      $ contract

contractParams :: String -> Contract.Config.ContractParams
contractParams bfApiKey =
  let
    pisaPreprodWalletSpec = Just $ UseMnemonic
      (MnemonicFile userMnemonicFile)
      { accountIndex: zero, addressIndex: zero }
      WithStakeKey
    blockFrostParams = mkBlockfrostBackendParams $
      { blockfrostApiKey: Just bfApiKey
      , blockfrostConfig: blockfrostPublicPreprodServerConfig
      , confirmTxDelay: defaultConfirmTxDelay
      }
  in
    Contract.Config.testnetConfig
      { walletSpec = pisaPreprodWalletSpec
      , backendParams = blockFrostParams
      }

userAddrAcc1 :: String
userAddrAcc1 =
  "\"addr_test1qrehsphwfepck4zzaptegyeau5ujj2xp5cjx3r8hajrjhqr2saysw7z6pdux5647rlu2tjdrze4mftsagr5ca3gkf4lsg5rpgh\""

pisaRefStr :: String
pisaRefStr =
  "{\"index\":1,\"transactionId\":\"4d1e7a08c598eeabcf34521a29f9604186c22408bdb4695589a2ee43a6ca52ec\"}"

contract :: Contract Unit
contract = do
  pisaRef <- fromJson "Pisa position ref" pisaRefStr

  addr <- fromJson "bech32 address" userAddrAcc1

  currSymbol <- fromJson "currency symbol"
    "\"1b650ba85f6590eebebe138cce94d96c62fcc332bbcfb3d9b3a11f33\""
  tokName <- mkAssetNameC "SwapTokenOne"
  let
    testValueToSend = CV.singleton currSymbol tokName (BigNum.fromInt 11)
    paramsSwapAsset = Asset currSymbol tokName

  unbalancedTx <- buildTx
    [ Pay $ TransactionOutput
        { address: addr
        , amount: testValueToSend
        , datum: Nothing
        , scriptRef: Nothing
        }
    ]

  pisaBalancedTx <- balanceTxWithPisa
    "http://localhost:8088/ws"
    { positionRef: pisaRef
    , swapAssets: [ paramsSwapAsset ]
    }
    unbalancedTx

  case pisaBalancedTx of
    Left e -> logError' $ "Failed to balance Tx with Pisa: " <> e
    Right txWithSwap -> do
      signed <- signTransaction txWithSwap
      tdId <- submit signed
      awaitTxConfirmed tdId

mkAssetNameC :: String -> Contract AssetName
mkAssetNameC str =
  liftContractM ("Cannot make token name from: " <> str)
    $ mkAssetName
    =<< byteArrayFromAscii str

fromJson ∷ forall a. DecodeAeson a ⇒ String → String → Contract a
fromJson what json =
  either
    ( \e -> throwContractError $ "some conversion failed: " <> what <> ": " <>
        show e
    )
    pure
    (parseJsonStringToAeson json >>= decodeAeson)
