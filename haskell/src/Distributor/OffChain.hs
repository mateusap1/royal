{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Distributor.OffChain where

import Control.Monad (forever, void)
import qualified Data.Map as HaskellMap
import Data.Monoid (Last (Last), (<>))
import Data.Text (Text)
import Distributor
import Distributor.OnChain
import Ledger.Ada (fromValue, getLovelace, lovelaceOf, lovelaceValueOf)
import Ledger.Constraints (ScriptLookups, TxConstraints)
import qualified Ledger.Constraints as Constraints
import Ledger.Contexts (TxOutRef)
import Ledger.Crypto (PubKeyHash, pubKeyHash)
import Ledger.Scripts (Datum (..), Redeemer (..), ValidatorHash, validatorHash)
import Ledger.Tx (ChainIndexTxOut, TxOut, toTxOut, txId, txOutValue)
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value (Value)
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract as Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
  ( Integer,
    foldl,
    foldr,
    fst,
    length,
    map,
    mconcat,
    mempty,
    return,
    snd,
    zip,
    ($),
    (*),
    (+),
    (++),
    (.),
    (<$>),
    (>),
  )
import qualified PlutusTx.Ratio as R
import Text.Printf (printf)
import qualified Prelude

-- The amount of ADA, a UTxO needs for it to be counted
utxoThereshold :: Integer
utxoThereshold = 0 -- In this case, any UTxO will pass

-- Returns all accounts, their Datum, the Value of fees they hold and their owners
findDistributors ::
  DistributionSettings ->
  Contract w s Text [(TxOutRef, ChainIndexTxOut)]
findDistributors ds = do
  -- All UTxOs located at the distributor address
  utxos <- utxosTxOutTxAt (distributionAddress ds)

  -- Return these UTxOs altered
  return $ foldr f [] (HaskellMap.toList utxos)
  where
    lovelaceAmt :: Integer
    lovelaceAmt = getLovelace (fromValue (dsMinUtxoValue ds))

    numOfReceivers :: Integer
    numOfReceivers = length (PlutusMap.toList (dsDistributionMapping ds))

    f ::
      (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) ->
      [(TxOutRef, ChainIndexTxOut)] ->
      [(TxOutRef, ChainIndexTxOut)]
    f (oref, (ciTxOut, _)) acc =
      -- The amount of ADA inside the contract must be greater than the minimum
      -- UTxO value times the number of receivers (since only then it could be
      -- valid)
      if getLovelace (fromValue val) > utxoThereshold
        then (oref, ciTxOut) : acc
        else acc
      where
        val :: Value
        val = txOutValue (toTxOut ciTxOut)

sendToDistributor ::
  DistributionSettings ->
  Integer ->
  Contract () DistributorSchema Text ()
sendToDistributor ds amt = do
  let -- The validator hash from our distribution script
      distributorValHash :: ValidatorHash
      distributorValHash = validatorHash (distributionValidator ds)

      distributorValue :: Value
      distributorValue = lovelaceValueOf amt

      tx ::
        TxConstraints
          (RedeemerType DistributionType)
          (DatumType DistributionType)
      tx = Constraints.mustPayToTheScript () distributorValue

  -- Submits the transaction to the blockchain
  ledgerTx <- submitTxConstraints (typedDistributionValidator ds) tx

  -- Waits for the transaction to be confirmed
  void $ Contract.awaitTxConfirmed $ txId ledgerTx

  Contract.logInfo @Prelude.String $
    printf "Successfully sent %s to distributor" (Prelude.show amt)

distribute :: DistributionSettings -> Contract () DistributorSchema Text ()
distribute ds = do
  pkh <- pubKeyHash <$> Contract.ownPubKey

  distributors <- findDistributors ds

  let -- The validator hash from our distribution script
      distributorValHash :: ValidatorHash
      distributorValHash = validatorHash (distributionValidator ds)

      distributorReferences :: [TxOutRef]
      distributorReferences = map fst distributors

      distributorOutputs :: [ChainIndexTxOut]
      distributorOutputs = map snd distributors

      totalAmt :: Integer
      totalAmt =
        foldl
          (\acc x -> acc + getLovelace (fromValue (txOutValue (toTxOut x))))
          0
          distributorOutputs

      expDis :: PlutusMap.Map PubKeyHash Integer
      expDis = expectedDistribution (dsDistributionMapping ds) totalAmt

      lookups :: ScriptLookups DistributionType
      lookups =
        Constraints.unspentOutputs
          (HaskellMap.fromList (zip distributorReferences distributorOutputs))
          <> Constraints.typedValidatorLookups (typedDistributionValidator ds)
          <> Constraints.otherScript (distributionValidator ds)

      tx ::
        TxConstraints
          (RedeemerType DistributionType)
          (DatumType DistributionType)
      tx =
        Constraints.mustBeSignedBy pkh
          <> mconcat
            [ Constraints.mustSpendScriptOutput
                oref
                (Redeemer $ PlutusTx.toBuiltinData Collect)
              | oref <- distributorReferences
            ]
          <> mconcat
            [ Constraints.mustPayToPubKey pkh (lovelaceValueOf amt)
              | (pkh, amt) <- PlutusMap.toList expDis
            ]
          <> Constraints.mustPayToOtherScript
            distributorValHash
            (Datum $ PlutusTx.toBuiltinData ())
            mempty

  -- Submits the transaction to the blockchain
  ledgerTx <- submitTxConstraintsWith @DistributionType lookups tx

  -- Waits for the transaction to be confirmed
  void $ Contract.awaitTxConfirmed $ txId ledgerTx

  Contract.logInfo @Prelude.String $
    "Successfully distributed "
      ++ Prelude.show (length distributors)
      ++ " distributors"

maliciousDistribute ::
  DistributionSettings ->
  PlutusMap.Map PubKeyHash Integer ->
  Contract () DistributorSchema Text ()
maliciousDistribute ds mlcMap = do
  pkh <- pubKeyHash <$> Contract.ownPubKey

  distributors <- findDistributors ds

  let -- The validator hash from our distribution script
      distributorValHash :: ValidatorHash
      distributorValHash = validatorHash (distributionValidator ds)

      distributorReferences :: [TxOutRef]
      distributorReferences = map fst distributors

      distributorOutputs :: [ChainIndexTxOut]
      distributorOutputs = map snd distributors

      totalAmt :: Integer
      totalAmt =
        foldl
          (\acc x -> acc + getLovelace (fromValue (txOutValue (toTxOut x))))
          0
          distributorOutputs

      mlcDis :: PlutusMap.Map PubKeyHash Integer
      mlcDis = expectedDistribution mlcMap totalAmt

      lookups :: ScriptLookups DistributionType
      lookups =
        Constraints.unspentOutputs
          (HaskellMap.fromList (zip distributorReferences distributorOutputs))
          <> Constraints.typedValidatorLookups (typedDistributionValidator ds)
          <> Constraints.otherScript (distributionValidator ds)

      tx ::
        TxConstraints
          (RedeemerType DistributionType)
          (DatumType DistributionType)
      tx =
        Constraints.mustBeSignedBy pkh
          <> mconcat
            [ Constraints.mustSpendScriptOutput
                oref
                (Redeemer $ PlutusTx.toBuiltinData Collect)
              | oref <- distributorReferences
            ]
          <> mconcat
            [ Constraints.mustPayToPubKey pkh (lovelaceValueOf amt)
              | (pkh, amt) <- PlutusMap.toList mlcDis
            ]
          <> Constraints.mustPayToOtherScript
            distributorValHash
            (Datum $ PlutusTx.toBuiltinData ())
            mempty

  -- Submits the transaction to the blockchain
  ledgerTx <- submitTxConstraintsWith @DistributionType lookups tx

  -- Waits for the transaction to be confirmed
  void $ Contract.awaitTxConfirmed $ txId ledgerTx

  Contract.logInfo @Prelude.String $
    "Successfully distributed "
      ++ Prelude.show (length distributors)
      ++ " distributors"

type DistributorSchema =
  Endpoint "distributor-send" (DistributionSettings, Integer)
    .\/ Endpoint "distribute" DistributionSettings
    .\/ Endpoint
      "malicious-distribute"
      (DistributionSettings, PlutusMap.Map PubKeyHash Integer)

distributorEndpoints :: Contract () DistributorSchema Text ()
distributorEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        sendToDistributor' `select` distribute' `select` maliciousDistribute'
  where
    sendToDistributor' =
      endpoint @"distributor-send" $ \(ds, amt) -> sendToDistributor ds amt
    distribute' = endpoint @"distribute" $ \ds -> distribute ds
    maliciousDistribute' =
      endpoint @"malicious-distribute" $ \(ds, mp) -> maliciousDistribute ds mp