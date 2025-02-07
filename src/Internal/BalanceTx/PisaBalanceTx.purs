module Internal.BalanceTx.PisaBalanceTx
  ( balanceTxWithPisa
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types (CborBytes(..))
import Contract.Monad (Contract)
import Contract.Transaction (Transaction)
import Contract.Wallet as Wallet
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.BalanceTx.PisaBalanceTx.Types
  ( BalancerResponse(..)
  , PisaBalanceArgs
  , PisaBalancingError(..)
  , PisaRequest
  , WsPath
  , mkRequest
  )
import Ctl.Internal.BalanceTx.PisaBalanceTx.WebSocket (singleWsCall)
import Ctl.Internal.Helpers (liftedM)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.ByteArray (hexToByteArray)
import Effect.Exception (error)

-- TODO to think: (Pisa): Atlas IO handler accepts only single collateral or none
-- TODO: collateral will become unspendable when provided to Atlas,
-- this may prevent balancing in some edge cases.
-- But if no collateral provided and Atlas picks collateral by itself, input can be spent as well.
-- Make collateral optional and supplied by the caller of balanceTxWithPisa?
-- or make separate `balanceTxWithPisaWithCollateral`? or...
balanceTxWithPisa
  :: WsPath
  -> PisaBalanceArgs
  -> Transaction
  -> Contract (Either PisaBalancingError Transaction)
balanceTxWithPisa wsPath pisaArgs tx = do
  changeAddress <- Wallet.getChangeAddress
  collateral <- getCollateral
  addresses <- Wallet.getWalletAddresses

  pisaRequest <-
    mkRequest
      pisaArgs
      tx
      addresses
      changeAddress
      collateral

  ethTx <- singlePisaBalanceWsCall wsPath pisaRequest
  pure ethTx
  where

  getCollateral = do
    let
      noCollateralError =
        error "Collateral is required for Pisa balancing" -- TODO: make type for Pisa balancing errors
    collaterals <- liftedM noCollateralError Wallet.getWalletCollateral
    case Array.uncons collaterals of
      Just { head: txIn, tail: _ } -> pure $ (unwrap txIn).input
      Nothing -> throwError noCollateralError

singlePisaBalanceWsCall
  :: String
  -> PisaRequest
  -> Contract (Either PisaBalancingError Transaction)
singlePisaBalanceWsCall wsUrl req = do
  resp <- bimap ProtocolMessageParsingError identity <$> singleWsCall wsUrl req
  pure $ case resp of
    Right (BalanceSuccess pisaResp) -> do
      when (req.requestId /= pisaResp.requestId) $
        Left (ResponseDoesNotMatchRequest req.requestId pisaResp.requestId)
      parseTx pisaResp.balancedCbor
    Right BalanceError -> Left $ PlaceholderErr "Pisa balance Error"
    Right BalanceFailure -> Left $ PlaceholderErr "Pisa balance Failure"
    Left other -> Left other

  where
  parseTx cborHex =
    note (FailedToPArseBalancedCbor cborHex)
      (CborBytes <$> hexToByteArray cborHex)
      >>= (note (FailedToPArseBalancedCbor cborHex) <<< decodeCbor)