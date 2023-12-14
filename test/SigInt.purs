module Test.SigInt where

import Contract.Prelude

import Contract.Config (ServerConfig)
import Contract.Log (logInfo')
import Contract.Monad (runContractInEnv)
import Contract.Test (InitialUTxOs, withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.Wallet (getWalletUtxos)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpretWithConfig)
import Data.BigInt (fromInt) as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.UInt as UInt
import Effect.Aff (delay, launchAff_)
import Mote (test)
import Test.Spec.Runner (defaultConfig)

main :: Effect Unit
main = launchAff_ $
  interpretWithConfig
    defaultConfig { timeout = Just $ wrap 600_000.0, exit = true }
    suite

suite :: TestPlanM (Aff Unit) Unit
suite =
  test
    "interruption test" $ do
    withPlutipContractEnv ctcPlutipConfig testDistribution
      \env wallet -> do
        do
          runContractInEnv env $ withKeyWallet wallet do
            utxos1 <- getWalletUtxos
            logInfo' $ "utxos1: " <> show utxos1

            logInfo' "starting deplay 10s - press CTRL + C"
            liftAff $ delay (Milliseconds (10000.0 :: Number))
            logInfo' "delay end"

            utxos2 <- getWalletUtxos
            logInfo' $ "utxos2: " <> show utxos2

            logInfo' "test end"

testDistribution :: InitialUTxOs
testDistribution =
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigInt.fromInt 2_000_000_000
      , BigInt.fromInt 1_000_000_000
      ]
  in
    distribution

ctcPlutipConfig :: PlutipConfig
ctcPlutipConfig =
  { host: localHost
  , port: UInt.fromInt 8082
  , logLevel: Info --todo: put bacl debug
  , hooks:
      { beforeInit: Nothing
      , beforeSign: Nothing
      , onError: Nothing
      , onSuccess: Nothing
      , onSubmit: Nothing
      }
  , ogmiosConfig: mkServerConfig 1338
  , kupoConfig: mkServerConfig 1442
  , customLogger: Nothing
  , suppressLogs: false
  , clusterConfig:
      { slotLength: Seconds 1.2
      , epochSize: Nothing
      , maxTxSize: Just $ UInt.fromInt 80000
      , raiseExUnitsToMax: false
      }
  }

mkServerConfig :: Int -> ServerConfig
mkServerConfig port =
  { port: UInt.fromInt port
  , host: localHost
  , secure: false
  , path: Nothing
  }

localHost :: String
localHost = "127.0.0.1"
