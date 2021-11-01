{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Distributor where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger ()
import Ledger.Ada (lovelaceValueOf)
import Ledger.Contexts (TxInfo, valuePaidTo)
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts ()
import Ledger.Value (Value, geq)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
  ( Bool (..),
    Eq,
    Integer,
    all,
    sum,
    (&&),
    (*),
    (==),
  )
import qualified PlutusTx.Ratio as R
import qualified Prelude

-- The current minimum utxo value (2021-10-18)
{-# INLINEABLE minUtxoValue #-}
minUtxoValue :: Value
minUtxoValue = lovelaceValueOf 34482

-- A data type that will map each reaciving address to it's receiving weight
type DistributionMapping = PlutusMap.Map PubKeyHash Integer

data DistributionSettings = DistributionSettings
  { dsMinUtxoValue :: !Value,
    dsDistributionMapping :: !DistributionMapping
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq DistributionSettings where
  {-# INLINEABLE (==) #-}
  (DistributionSettings muv dm) == (DistributionSettings muv' dm') =
    muv == muv' && dm == dm'

PlutusTx.unstableMakeIsData ''DistributionSettings
PlutusTx.makeLift ''DistributionSettings

-- Verify if a specific user is within the receivers of a specific distribution
{-# INLINEABLE isReceiver #-}
isReceiver :: DistributionSettings -> PubKeyHash -> Bool
isReceiver ds pkh = pkh `PlutusMap.member` dsDistributionMapping ds

-- Returns the expected distribution for a given distribution mapping and a
-- given total value
{-# INLINEABLE expectedDistribution #-}
expectedDistribution ::
  DistributionMapping ->
  Integer ->
  PlutusMap.Map PubKeyHash Integer
expectedDistribution dm totAmt = PlutusMap.mapWithKey transform dm
  where
    totWgt :: Integer
    totWgt = sum (PlutusMap.elems dm)

    transform :: PubKeyHash -> Integer -> Integer
    transform _ amt = R.round ((amt * totAmt) R.% totWgt)

-- Make sure every receiver received the values proportional to the distribution
{-# INLINEABLE validDistribution #-}
validDistribution :: DistributionSettings -> TxInfo -> Integer -> Bool
validDistribution ds info totAmt =
  all f (PlutusMap.toList (expectedDistribution (dsDistributionMapping ds) totAmt))
  where
    f :: (PubKeyHash, Integer) -> Bool
    f (pkh, amt) =
      (valuePaidTo info pkh `geq` lovelaceValueOf amt)
        && (lovelaceValueOf amt `geq` dsMinUtxoValue ds)

-- The distribution script possible actions

-- Collect should distribute the stored tokens according to the proportion
-- defined in DistributionMapping

-- Change pkh pkh' should change the distribution receiver from pkh to pkh', as
-- long as pkh signed the transaction
data DistributionRedeemer = Collect | Change PubKeyHash PubKeyHash
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq DistributionRedeemer where
  {-# INLINEABLE (==) #-}
  Change old new == Change old' new' = old == old' && new == new'
  Collect == Collect = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''DistributionRedeemer
