module Ctl.Internal.BalanceTx.PisaBalanceTx.WebSocket
  ( singleWsCall
  , singleWsCall'
  ) where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError
  , decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  , stringifyAeson
  )
import Contract.Monad (Contract)
import Ctl.Internal.Helpers (logString)
import Ctl.Internal.JsWebSocket
  ( _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _wsClose
  , _wsFinalize
  , _wsSend
  )
import Effect.Aff (Canceler(Canceler), makeAff)
import Effect.Exception (Error)

singleWsCall
  :: forall a b
   . EncodeAeson a
  => DecodeAeson b
  => String
  -> a
  -> Contract (Either JsonDecodeError b) -- TODO: better error type
singleWsCall wsUrl req = do
  rawResp <- singleWsCall' wsUrl (stringifyAeson $ encodeAeson req)
  let
    resp = parseJsonStringToAeson rawResp >>= decodeAeson
  pure resp

singleWsCall' :: String -> String -> Contract String
singleWsCall' wsUrl msg = liftAff $ sendSingleWsCallAff wsUrl msg

sendSingleWsCallAff ∷ String -> String -> Aff String
sendSingleWsCallAff wsUrl msg = makeAff $ sendSingleWsCall wsUrl msg

sendSingleWsCall
  :: String
  -> String
  -> (Either Error String -> Effect Unit)
  -> Effect Canceler
sendSingleWsCall wsUrl msg cb = do
  ws <- _mkWebSocket logger wsUrl
  void $ _onWsError ws $ \err -> do
    logger $ "WS error, closing: " <> err
    close ws
  _onWsConnect ws do
    logger "WS connected"
    _wsSend ws logger msg
    _onWsMessage ws logger $ \resp -> do
      close ws
      cb $ Right resp
  pure $ Canceler $ \err -> liftEffect do
    close ws
    cb $ Left $ err
  where
  close ws = _wsFinalize ws *> _wsClose ws
  logger = logString Debug Debug
