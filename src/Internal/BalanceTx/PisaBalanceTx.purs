module Internal.BalanceTx.PisaBalanceTx
  ( balanceTxWithPisa
  ) where

import Contract.Prelude
import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types (CborBytes(..))
import Contract.Monad (Contract, liftedE)
import Contract.Transaction (Transaction, TransactionInput(..))
import Contract.Wallet as Wallet
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Ctl.Internal.BalanceTx.PisaBalanceTx.Errors (PisaBalancingError(..))
import Ctl.Internal.BalanceTx.PisaBalanceTx.Types
  ( BalancerResponse(..)
  , PisaBalanceArgs
  , PisaRequest
  , WsPath
  , mkRequest
  )
import Ctl.Internal.BalanceTx.PisaBalanceTx.WebSocket (singleWsCall)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.ByteArray (hexToByteArray)

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
  addresses <- Wallet.getWalletAddresses

  runExceptT do
    collateral <- ExceptT $
      note PisaBalancingMissingCollateral <$> getCollateral

    pisaRequest <- lift $
      mkRequest
        pisaArgs
        tx
        addresses
        changeAddress
        collateral

    ExceptT $ singlePisaBalanceWsCall wsPath pisaRequest
  where

  getCollateral :: Contract (Maybe TransactionInput)
  getCollateral =
    Wallet.getWalletCollateral >>= \mc -> pure $ mc >>= \collaterals ->
      case Array.uncons collaterals of
        Just { head: txIn, tail: _ } -> Just (unwrap txIn).input
        Nothing -> Nothing

singlePisaBalanceWsCall
  :: String
  -> PisaRequest
  -> Contract (Either PisaBalancingError Transaction)
singlePisaBalanceWsCall wsUrl req = do
  resp <- bimap PisaResponseParsingError identity <$> singleWsCall wsUrl req
  pure $ case resp of
    Right (BalanceSuccess pisaResp) -> do
      when (req.requestId /= pisaResp.requestId) $
        Left (ResponseDoesNotMatchRequest req.requestId pisaResp.requestId)
      parseTx pisaResp.balancedCbor
    Right fail@(RequestFail _) -> Left $ PisaBackendError fail
    Right err@(PisaServiceError _) -> Left $ PisaBackendError err
    Left otherErr -> Left otherErr

  where
  parseTx cborHex =
    note (FailedToParseBalancedCbor cborHex)
      (CborBytes <$> hexToByteArray cborHex)
      >>= (note (FailedToParseBalancedCbor cborHex) <<< decodeCbor)
