module Ctl.Internal.BalanceTx.PisaBalanceTx.Types
  ( BalancerResponse(..)
  , OutRef
  , PisaBalanceArgs
  , PisaBalancingError(..)
  , PisaRequest
  , SwapAsset(..)
  , UID
  , mkRequest
  , WsPath
  ) where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(..)
  , decodeAeson
  , getField
  )
import Aeson as Aeson
import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.Types (CborBytes)
import Cardano.Types.Asset (Asset(AdaAsset, Asset))
import Cardano.Types.AssetName (unAssetName)
import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Transaction (Transaction, TransactionInput(..))
import Ctl.Internal.Service.Helpers (aesonObject)
import Data.ByteArray (byteArrayToHex)
import Data.Show.Generic (genericShow)
import Data.UInt as UInt
import Data.UUID (UUID)
import Data.UUID as UUID

data PisaBalancingError
  = ResponseDoesNotMatchRequest
      UID -- ^ request id
      UID -- ^ response id
  | BalancingFailed
  | PisaBalancingMissingCollateral -- TODO: see collateral comment for `balanceTxWithPisa`
  | FailedToPArseBalancedCbor String
  | ProtocolMessageParsingError JsonDecodeError
  | PlaceholderErr String

derive instance Generic PisaBalancingError _

instance Show PisaBalancingError where
  show = genericShow

type WsPath = String

type PisaBalanceArgs =
  { positionRef :: TransactionInput
  , swapAssets :: Array Asset
  }

-- *** Pisa WS stuff
type PisaRequest =
  { requestId :: UID
  , requestType :: String
  , payload ::
      { positionRef :: OutRef
      , swapAssets :: Array SwapAsset
      , unbalancedTxCbor :: CborBytes
      , userAddresses :: Array Address
      , userChangeAddress :: Address
      , userCollateral :: OutRef
      }
  }

mkRequest
  :: PisaBalanceArgs
  -> Transaction
  -> Array Address
  -> Address
  -> TransactionInput
  -> Contract PisaRequest
mkRequest pArgs tx userAddresses userChangeAddress userCollateral = do
  uid <- liftEffect UUID.genUUID
  pure $
    { requestId: UID uid
    , requestType: "balanceCbor"
    , payload:
        { positionRef: OutRef pArgs.positionRef
        , swapAssets: map SwapAsset pArgs.swapAssets
        , unbalancedTxCbor: encodeCbor tx
        , userAddresses: userAddresses
        , userChangeAddress: userChangeAddress
        , userCollateral: OutRef userCollateral
        }
    }

-- TODO: finish it
-- https://github.com/mlabs-haskell/pisa-fees?tab=readme-ov-file#balancing-via-web-socket---messages-examples

data BalancerResponse
  = BalanceSuccess
      { balancedCbor :: String
      , requestId :: UID

      }
  | BalanceError
  | BalanceFailure

-- *** Serialization

instance DecodeAeson BalancerResponse where
  decodeAeson = aesonObject \obj -> do
    status <- getField obj "status"
    (respData :: Aeson) <- getField obj "data"
    case status of
      "success" -> BalanceSuccess <$> (decodeAeson respData)
      "error" -> pure BalanceError
      "fail" -> pure BalanceFailure
      unknown -> Left $ TypeMismatch
        $ "Unknown status while parsing BalancerResponse: "
        <> unknown

newtype OutRef = OutRef TransactionInput

instance EncodeAeson OutRef where
  encodeAeson (OutRef (TransactionInput { index, transactionId })) =
    Aeson.fromString $ asCborToString transactionId <> "#" <> show
      (UInt.toInt index)

newtype UID = UID UUID

derive newtype instance Eq UID

instance Show UID where
  show (UID uuid) = UUID.toString uuid

instance EncodeAeson UID where
  encodeAeson = show >>> Aeson.fromString

instance DecodeAeson UID where
  decodeAeson a = do
    str <- note (TypeMismatch "Expected string") (Aeson.toString a)
    uuid <- note (TypeMismatch "Can't parseUUID") (UUID.parseUUID str)
    pure $ UID uuid

newtype SwapAsset = SwapAsset Asset

instance EncodeAeson SwapAsset where
  encodeAeson (SwapAsset a) = case a of
    AdaAsset -> Aeson.fromString ""
    (Asset pHash tName) -> Aeson.fromString
      $ asCborToString pHash
      <> "."
      <> byteArrayToHex (unAssetName tName)

asCborToString ∷ forall (a ∷ Type). AsCbor a ⇒ a -> String
asCborToString = byteArrayToHex <<< unwrap <<< encodeCbor
