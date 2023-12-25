-- | A module for shared types across Cardano.Types.Transaction and
-- | Plutus.Types.Transaction.
module Ctl.Internal.Types.Transaction
  ( DataHash(DataHash)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , encodeAeson
  , toStringifiedNumbersJson
  )
import Ctl.Internal.FromData (class FromData, fromData)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.BigNum (zero) as BigNum
import Ctl.Internal.Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  )
import Ctl.Internal.Types.PlutusData (PlutusData(Constr))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt, toInt)
import Test.QuickCheck.Arbitrary
  ( class Arbitrary
  , class Coarbitrary
  , coarbitrary
  )
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype TransactionInput = TransactionInput
  { transactionId :: TransactionHash
  , index :: UInt
  }

derive instance Newtype TransactionInput _
derive instance Generic TransactionInput _
derive newtype instance Eq TransactionInput
derive newtype instance EncodeAeson TransactionInput
derive newtype instance DecodeAeson TransactionInput

instance EncodeJson TransactionInput where
  encodeJson a = toStringifiedNumbersJson  $ encodeAeson a

-- Potential fix me: the below is based on a small sample of smart contract
-- transactions, so fix this as required.
-- Not newtype derived this because it is not lexicographical as `index` is tested
-- before `transactionId`. We require lexicographical order over hexstring
-- `TransactionHash`, then `index`, seemingly inline with Cardano/Plutus.
instance Ord TransactionInput where
  compare (TransactionInput txInput) (TransactionInput txInput') =
    case compare txInput.transactionId txInput'.transactionId of
      EQ -> compare txInput.index txInput'.index
      x -> x

instance Show TransactionInput where
  show = genericShow

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance FromData TransactionInput where
  fromData (Constr n [ txId, idx ]) | n == BigNum.zero =
    TransactionInput <$>
      ({ transactionId: _, index: _ } <$> fromData txId <*> fromData idx)
  fromData _ = Nothing

-- `Constr` is used for indexing, and `TransactionInput` is always zero-indexed
instance ToData TransactionInput where
  toData (TransactionInput { transactionId, index }) =
    Constr BigNum.zero [ toData transactionId, toData index ]

instance Coarbitrary TransactionInput where
  coarbitrary (TransactionInput input) generator =
    coarbitrary (toInt input.index) $ coarbitrary input.transactionId generator

-- | 32-bytes blake2b256 hash of a tx body.
-- | NOTE. Plutus docs might incorrectly state that it uses
-- |       SHA256 for this purposes.
newtype TransactionHash = TransactionHash ByteArray

derive instance Generic TransactionHash _
derive instance Newtype TransactionHash _
derive newtype instance Eq TransactionHash
derive newtype instance EncodeAeson TransactionHash
derive newtype instance DecodeAeson TransactionHash

-- This is not newtyped derived because it will be used for ordering a
-- `TransactionInput`, we want lexicographical ordering on the hexstring.
instance Ord TransactionHash where
  compare (TransactionHash h) (TransactionHash h') =
    compare (byteArrayToHex h) (byteArrayToHex h')

instance Show TransactionHash where
  show = genericShow

-- Plutus actually has this as a zero indexed record
instance FromData TransactionHash where
  fromData (Constr n [ bytes ]) | n == BigNum.zero = TransactionHash <$>
    fromData bytes
  fromData _ = Nothing

-- Plutus actually has this as a zero indexed record
instance ToData TransactionHash where
  toData (TransactionHash bytes) = Constr BigNum.zero [ toData bytes ]

instance Arbitrary TransactionHash where
  arbitrary =
    wrap <<< byteArrayFromIntArrayUnsafe <$> vectorOf 32 (chooseInt 0 255)

instance Coarbitrary TransactionHash where
  coarbitrary (TransactionHash bytes) generator = coarbitrary bytes generator

newtype DataHash = DataHash ByteArray

derive instance Generic DataHash _
derive instance Newtype DataHash _
derive newtype instance Eq DataHash
derive newtype instance FromData DataHash
derive newtype instance Ord DataHash
derive newtype instance ToData DataHash
derive newtype instance DecodeAeson DataHash
derive newtype instance EncodeAeson DataHash

instance EncodeJson DataHash where
  encodeJson a = toStringifiedNumbersJson $ encodeAeson a

instance Show DataHash where
  show = genericShow
