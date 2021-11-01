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

module Spec.Trace where

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Distributor
import Distributor.OffChain
import Ledger
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.TimeSlot
import Ledger.Value (AssetClass (AssetClass), assetClass, assetClassValue)
import Plutus.Contract.Test (Wallet (Wallet), knownWallet, walletPubKey)
import Plutus.Trace.Emulator as Emulator
  ( ContractHandle,
    EmulatorConfig (EmulatorConfig),
    EmulatorTrace,
    activateContractWallet,
    callEndpoint,
    observableState,
    runEmulatorTraceIO',
    waitNSlots,
  )
import PlutusTx.AssocMap as M
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinByteString,
    Either (Left),
    Integer,
    Maybe (Just, Nothing),
    Semigroup ((<>)),
    ($),
    (++),
    (-),
  )
import qualified PlutusTx.Ratio as R
import Spec.Sample
import Test.Tasty ()
import Prelude (IO, Show (..), String)
import qualified Prelude

distributorSendTrace ::
  ContractHandle () DistributorSchema Text ->
  DistributionSettings ->
  Integer ->
  EmulatorTrace ()
distributorSendTrace handle ds amt =
  callEndpoint @"distributor-send" handle (ds, amt)

distributeTrace ::
  ContractHandle () DistributorSchema Text ->
  DistributionSettings ->
  EmulatorTrace ()
distributeTrace handle ds = do
  callEndpoint @"distribute" handle ds

maliciousDistributeTrace ::
  ContractHandle () DistributorSchema Text ->
  DistributionSettings ->
  PlutusMap.Map PubKeyHash Integer ->
  EmulatorTrace ()
maliciousDistributeTrace handle ds mp = do
  callEndpoint @"malicious-distribute" handle (ds, mp)