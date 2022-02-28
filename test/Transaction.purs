module Test.Transaction (suite) where

import Prelude

import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, over)
import Data.Tuple.Nested ((/\))
import Deserialization.PlutusData as Deserialization.PlutusData
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Helpers (fromRightEff)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Serialization.WitnessSet as Serialization.WitnessSet
import Transaction (attachDatum)
import Types.Transaction as Transaction
import Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  )
import Types.PlutusData (Datum(Datum), PlutusData(Integer))

suite :: TestPlanM Unit
suite = group "attach datums to tx" $ do
  test "datum should be correctly attached" testAttachDatum
  test "existing witnesses should be preserved" testPreserveWitness

testAttachDatum :: Aff Unit
testAttachDatum = liftEffect $
  attachDatum datum tx >>= case _ of
    Left e -> throw $ "Failed to attach datum: " <> show e
    Right (Transaction { witness_set: TransactionWitnessSet ws }) ->
      case ws.plutus_data of
        Just [ pd ] -> do
          pd' <- checkDatum pd
          pd' `shouldEqual` Just (unwrap datum)
        Just _ -> throw "Incorrect number of datums attached"
        Nothing -> throw "Datum wasn't attached"
  where
  tx :: Transaction
  tx = mempty

  datum :: Datum
  datum = Datum $ Integer $ BigInt.fromInt 1

testPreserveWitness :: Aff Unit
testPreserveWitness = liftEffect $ do
  Transaction { witness_set: TransactionWitnessSet { plutus_data, vkeys } } <-
    fromRightEff =<< attachDatum datum tx
  case plutus_data /\ vkeys of
    Just [ pd ] /\ Just vs@[ _ ] -> do
      pd' <- checkDatum pd
      pd' `shouldEqual` Just (unwrap datum)
      vk' <- Deserialization.WitnessSet.convertVkeyWitnesses <$>
        Serialization.WitnessSet.convertVkeywitnesses vs
      vk' `shouldEqual` [ vk ]
    Just _ /\ Just _ -> throw "Incorrect number of witnesses"
    Nothing /\ _ -> throw "Datum wasn't attached"
    _ /\ Nothing -> throw "Vkey witness wasn't preserved"
  where
  tx :: Transaction
  tx = over Transaction _ { witness_set = initialWitnessSet }
    $ (mempty :: Transaction)

  datum :: Datum
  datum = Datum $ Integer $ BigInt.fromInt 1

  initialWitnessSet :: TransactionWitnessSet
  initialWitnessSet = over TransactionWitnessSet _ { vkeys = Just [ vk ] }
    $ (mempty :: TransactionWitnessSet)

  vk :: Vkeywitness
  vk = Vkeywitness
    ( (Vkey (PublicKey "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j")) /\
        (Ed25519Signature "ed25519_sig1clmhgxx9e9t24wzgkmcsr44uq98j935evsjnrj8nn7ge08qrz0mgdxv5qtz8dyghs47q3lxwk4akq3u2ty8v4egeqvtl02ll0nfcqqq6faxl6")
    )

checkDatum :: Transaction.PlutusData -> Effect (Maybe PlutusData)
checkDatum = map Deserialization.PlutusData.convertPlutusData
  <<< Serialization.WitnessSet.convertPlutusData