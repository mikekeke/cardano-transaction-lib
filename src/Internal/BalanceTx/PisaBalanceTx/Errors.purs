module Ctl.Internal.BalanceTx.PisaBalanceTx.Errors
  ( PisaBalancingError(..)
  ) where

import Contract.Prelude

import Aeson (JsonDecodeError)
import Ctl.Internal.BalanceTx.PisaBalanceTx.Types (BalancerResponse, UID)
import Data.Show.Generic (genericShow)

data PisaBalancingError
  = ResponseDoesNotMatchRequest
      UID -- ^ request id
      UID -- ^ response id
  | PisaBackendError BalancerResponse
  | PisaBalancingMissingCollateral -- TODO: see collateral comment for `balanceTxWithPisa`
  | FailedToParseBalancedCbor String
  | PisaResponseParsingError JsonDecodeError
  | PlaceholderErr String

derive instance Generic PisaBalancingError _

instance Show PisaBalancingError where
  show = genericShow