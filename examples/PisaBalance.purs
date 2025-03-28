-- | This module demonstrates the use Pisa balancer
module Ctl.Examples.PisaBalance
  ( main
  , contract
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
  "credentials": {
    "blockfrostApiKey": "...",
    "userMnemonicFile": "..."
  },
  "pisaBackendWsPath": "wss://pisa-backend-url:port/ws",
  "positionRef": {
    "index": 1,
    "transactionId": "37891a33521af4889e896f44106b14e994dc3f7b04e7c6ffe119b633c962aa54"
  },
  "sendInfo": {
    "addressSendTo": "addr_test1qrehsphwfepck4zzaptegyeau5ujj2xp5cjx3r8hajrjhqr2saysw7z6pdux5647rlu2tjdrze4mftsagr5ca3gkf4lsg5rpgh",
    "amountToSend": 4,
    "assetPolicy": "1b650ba85f6590eebebe138cce94d96c62fcc332bbcfb3d9b3a11f33",
    "assetName": "SendTokenOne"
  },
  "swapConf": {
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
      Right (conf :: PisaExampleConf) -> void
        $ Contract.Monad.runContract (contractParams conf.credentials)
        $ contract
            conf.pisaBackendWsPath
            conf.positionRef
            (conf.swapConf.assetPolicy /\ conf.swapConf.assetName)
            (conf.sendInfo.assetPolicy /\ conf.sendInfo.assetName)
            conf.sendInfo.addressSendTo
            conf.sendInfo.amountToSend

-- | This contract uses CTL to build unbalanced transaction which then
-- | balanced using Pisa Fees backend server.
-- |
-- | This contract is made for demo purposes and limited to sending some tokens from one
-- | address to another using same or another token(s) to perform fee swap with Pisa script.
-- contract :: PisaExampleConf -> Contract Unit
contract
  ∷ String
  -> TransactionInput
  -> (ScriptHash /\ String)
  -> (ScriptHash /\ String)
  -> Address
  -> Int
  -> Contract Unit
contract
  pisaWsPath
  pisaPositionRef
  (swapCurrSymbol /\ swapAssetName)
  (sendCurrSymbol /\ sendAssetName)
  addressSendTo
  amountToSend = do
  swapTokenName <- mkAssetNameC swapAssetName
  let
    paramsSwapAsset = Asset swapCurrSymbol swapTokenName

  -- send params
  sendTokenName <- mkAssetNameC sendAssetName
  let
    testValueToSend = CV.singleton sendCurrSymbol sendTokenName
      (BigNum.fromInt amountToSend)

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
  { credentials :: Credentials
  , pisaBackendWsPath :: String
  , positionRef :: TransactionInput
  , sendInfo :: TestSendInfo
  , swapConf :: TestSwapConf
  }

type Credentials =
  { userMnemonicFile :: String
  , blockfrostApiKey :: String
  }

type TestSwapConf =
  { assetPolicy :: ScriptHash
  , assetName :: String
  }

type TestSendInfo =
  { addressSendTo :: Address
  , assetPolicy :: ScriptHash
  , assetName :: String
  , amountToSend :: Int
  }

contractParams :: Credentials -> Contract.Config.ContractParams
contractParams credentials =
  let
    pisaPreprodWalletSpec = Just $ UseMnemonic
      (MnemonicFile credentials.userMnemonicFile)
      { accountIndex: zero, addressIndex: zero }
      WithStakeKey
    blockFrostParams = mkBlockfrostBackendParams $
      { blockfrostApiKey: Just credentials.blockfrostApiKey
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
