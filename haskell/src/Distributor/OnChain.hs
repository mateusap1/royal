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

module Distributor.OnChain where

import Distributor
  ( DistributionRedeemer (Collect),
    DistributionSettings,
    validDistribution,
  )
import Ledger.Ada (fromValue, getLovelace)
import Ledger.Address (Address, scriptAddress, toValidatorHash)
import Ledger.Contexts
  ( ScriptContext (scriptContextTxInfo),
    TxInInfo,
    TxInfo,
    TxOut (txOutAddress),
    findOwnInput,
    ownHash,
    scriptOutputsAt,
    txInInfoResolved,
    txInfoInputs,
  )
import Ledger.Scripts (DatumHash)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (Value)
import qualified PlutusTx
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    Integer,
    Maybe (..),
    filter,
    foldr,
    head,
    not,
    snd,
    traceError,
    traceIfFalse,
    (-),
    (.),
    (==),
    (||),
  )
import Utils (scriptInputValuesAt)

{-# INLINEABLE mkDistributionValidator #-}
mkDistributionValidator ::
  DistributionSettings ->
  () ->
  DistributionRedeemer ->
  ScriptContext ->
  Bool
mkDistributionValidator ds () Collect ctx =
  not isFirstInput
    || traceIfFalse "Invalid distribution" (validDistribution ds info totalAmount)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "No input"
      Just i -> i

    isFirstInput :: Bool
    isFirstInput = head f == ownInput
      where
        f :: [TxInInfo]
        f =
          filter
            ( \i ->
                (toValidatorHash . txOutAddress . txInInfoResolved) i
                  == Just (ownHash ctx)
            )
            (txInfoInputs info)

    distributorScriptInputs :: [Value]
    distributorScriptInputs = scriptInputValuesAt (ownHash ctx) info

    distributorScriptOutputs :: [(DatumHash, Value)]
    distributorScriptOutputs = scriptOutputsAt (ownHash ctx) info

    totalAmount :: Integer
    totalAmount =
      foldr ((+) . getLovelace . fromValue) 0 distributorScriptInputs
        - foldr ((+) . getLovelace . fromValue . snd) 0 distributorScriptOutputs

data DistributionType

instance Scripts.ValidatorTypes DistributionType where
  type DatumType DistributionType = ()
  type RedeemerType DistributionType = DistributionRedeemer

typedDistributionValidator ::
  DistributionSettings ->
  Scripts.TypedValidator DistributionType
typedDistributionValidator ds =
  Scripts.mkTypedValidator @DistributionType
    ( $$(PlutusTx.compile [||mkDistributionValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode ds
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @DistributionRedeemer

distributionValidator :: DistributionSettings -> Scripts.Validator
distributionValidator = Scripts.validatorScript . typedDistributionValidator

distributionAddress :: DistributionSettings -> Address
distributionAddress = scriptAddress . distributionValidator