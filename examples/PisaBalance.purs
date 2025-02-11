-- | This module demonstrates the use Pisa balancer
module Ctl.Examples.PisaBalance
  ( main
  , contract
  , PisaExampleConf
  , TestSendInfo
  , TestSwapConf
  ) where

import Contract.Prelude

import Aeson (decodeAeson, parseJsonStringToAeson)
import Cardano.Transaction.Builder (TransactionBuilderStep(Pay))
import Cardano.Types (Address, AssetName, ScriptHash, TransactionInput)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value as CV
import Contract.Config
  ( MnemonicSource(MnemonicFile)
  , StakeKeyPresence(WithStakeKey)
  , WalletSpec(UseMnemonic)
  , blockfrostPublicPreprodServerConfig
  , defaultConfirmTxDelay
  , mkBlockfrostBackendParams
  )
import Contract.Config as Contract.Config
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractM)
import Contract.Monad as Contract.Monad
import Contract.Transaction (awaitTxConfirmed, buildTx, signTransaction, submit)
import Data.Array as Array
import Data.ByteArray (byteArrayFromAscii)
import Effect.Exception (throw)
import Internal.BalanceTx.PisaBalanceTx (balanceTxWithPisa)
import Node.Encoding as Encoding
import Node.FS.Aff (readTextFile)
import Node.Process (argv)

-- *** Run
-- spago run -m Ctl.Examples.PisaBalance -b "path-to-config"

-- *** Example config
{-
{
  "blockfrostApiKey": "...",
  "userMnemonicFile": "...",
  "testSendInfo": {
    "addressSendTo": "addr_test1qrehsphwfepck4zzaptegyeau5ujj2xp5cjx3r8hajrjhqr2saysw7z6pdux5647rlu2tjdrze4mftsagr5ca3gkf4lsg5rpgh",
    "amountToSend": 12,
    "assetPolicy": "1b650ba85f6590eebebe138cce94d96c62fcc332bbcfb3d9b3a11f33",
    "assetName": "SendTokenOne"
  },
  "testSwapConf": {
    "pisaBackendWsPath": "http://localhost:8088/ws",
    "positionRef": {
      "index": 1,
      "transactionId": "b5f831a0d8707c4d60f4cfbba6e51770970cbf325eb76a8d73762c33e42a462b"
    },
    "assetPolicy": "1b650ba85f6590eebebe138cce94d96c62fcc332bbcfb3d9b3a11f33",
    "assetName": "SwapTokenOne"
  }
}
-}

main :: Effect Unit
main = do
  args <- argv
  configFilePath <- case Array.index args 2 of
    Nothing -> throw "Path to config file required to run example contract"
    Just p -> pure p
  Contract.Monad.launchAff_ do
    exampleConf <- readTextFile Encoding.UTF8 configFilePath
    case parseJsonStringToAeson exampleConf >>= decodeAeson of
      Left e -> log $ "Failed to parse Pisa example config: " <> show e
      Right conf -> void
        $ Contract.Monad.runContract (contractParams conf)
        $ contract conf

-- | This contract uses CTL to build unbalanced transaction which then
-- | balanced using Pisa Fees backend server.
-- |
-- | This contract is made for demo purposes and limited to sending some tokens from one
-- | address to another using same or another token(s) to perform fee swap with Pisa script.
contract :: PisaExampleConf -> Contract Unit
contract conf = do
  -- swap params
  swapTokenName <- mkAssetNameC conf.testSwapConf.assetName
  let
    pisaWsPath = conf.testSwapConf.pisaBackendWsPath
    pisaPositionRef = conf.testSwapConf.positionRef
    swapCurrSymbol = conf.testSwapConf.assetPolicy
    paramsSwapAsset = Asset swapCurrSymbol swapTokenName

  -- send params
  sendTokenName <- mkAssetNameC conf.testSendInfo.assetName
  let
    addressSendTo = conf.testSendInfo.addressSendTo
    sendCurrSymbol = conf.testSendInfo.assetPolicy
    testValueToSend = CV.singleton sendCurrSymbol sendTokenName
      (BigNum.fromInt conf.testSendInfo.amountToSend)

  -- going Pisa
  unbalancedTx <- buildTx
    [ Pay $ TransactionOutput
        { address: addressSendTo
        , amount: testValueToSend
        , datum: Nothing
        , scriptRef: Nothing
        }
    ]

  pisaBalancedTx <- balanceTxWithPisa
    pisaWsPath
    { positionRef: pisaPositionRef
    , swapAssets: [ paramsSwapAsset ]
    }
    unbalancedTx

  case pisaBalancedTx of
    Left e -> logError' $ "Failed to balance Tx with Pisa: " <> show e
    Right txWithSwap -> do
      signed <- signTransaction txWithSwap
      tdId <- submit signed
      awaitTxConfirmed tdId

type PisaExampleConf =
  { userMnemonicFile :: String
  , blockfrostApiKey :: String
  , testSendInfo :: TestSendInfo
  , testSwapConf :: TestSwapConf
  }

type TestSwapConf =
  { pisaBackendWsPath :: String
  , positionRef :: TransactionInput
  , assetPolicy :: ScriptHash
  , assetName :: String
  }

type TestSendInfo =
  { addressSendTo :: Address
  , assetPolicy :: ScriptHash
  , assetName :: String
  , amountToSend :: Int
  }

contractParams :: PisaExampleConf -> Contract.Config.ContractParams
contractParams conf =
  let
    pisaPreprodWalletSpec = Just $ UseMnemonic
      (MnemonicFile conf.userMnemonicFile)
      { accountIndex: zero, addressIndex: zero }
      WithStakeKey
    blockFrostParams = mkBlockfrostBackendParams $
      { blockfrostApiKey: Just conf.blockfrostApiKey
      , blockfrostConfig: blockfrostPublicPreprodServerConfig
      , confirmTxDelay: defaultConfirmTxDelay
      }
  in
    Contract.Config.testnetConfig
      { walletSpec = pisaPreprodWalletSpec
      , backendParams = blockFrostParams
      }

mkAssetNameC :: String -> Contract AssetName
mkAssetNameC str =
  liftContractM ("Failed to make token name from: " <> str)
    $ mkAssetName
    =<< byteArrayFromAscii str
