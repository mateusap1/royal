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

module Spec.Test where

import Control.Lens
import Control.Monad hiding (fmap)
import Control.Monad.Freer.Extras as Extras
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Distributor
import Distributor.OnChain
import Ledger
import Ledger.Ada as Ada
import Ledger.Value
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude
import Spec.Example
import Spec.Run (abstractConfig)
import Spec.Sample
import qualified Test.HUnit as HUnit
import Test.Tasty
import Prelude (IO, Show (..), String)

tests :: TestTree
tests =
  testGroup
    "Distributor"
    [ myDistributionTest,
      simpleDistributionTest,
      multipleUtxosTest,
      belowMinimumTest,
      maliciousDistributionTest
    ]

myDistributionTest :: TestTree
myDistributionTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ abstractConfig [1 .. 3])
    "my distribution trace"
    ( walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-6_000_000))
        .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 3_000_000)
        .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf 3_000_000)
    )
    simpleExample

simpleDistributionTest :: TestTree
simpleDistributionTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ abstractConfig [1 .. 5])
    "simple distribution trace"
    ( walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-100_000_000))
        .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 10_000_000)
        .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf 10_000_000)
        .&&. walletFundsChange (knownWallet 4) (Ada.lovelaceValueOf 40_000_000)
        .&&. walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf 40_000_000)
    )
    simpleExample

multipleUtxosTest :: TestTree
multipleUtxosTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ abstractConfig [1 .. 7])
    "multiple utxos distribution trace"
    ( walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-50_000_000))
        .&&. walletFundsChange (knownWallet 7) (Ada.lovelaceValueOf (-50_000_000))
        .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 10_000_000)
        .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf 10_000_000)
        .&&. walletFundsChange (knownWallet 4) (Ada.lovelaceValueOf 40_000_000)
        .&&. walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf 40_000_000)
    )
    multipleUtxosExample

belowMinimumTest :: TestTree
belowMinimumTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ abstractConfig [1 .. 5])
    "below minimum utxo value trace"
    ( walletFundsChange
        (knownWallet 1)
        (Ada.lovelaceValueOf (- (getLovelace (fromValue minUtxoValue) * 10 - 100)))
        .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 4) (Ada.lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf 0)
    )
    belowMinimumExample

maliciousDistributionTest :: TestTree
maliciousDistributionTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ abstractConfig [1 .. 5])
    "malicious distribution trace"
    ( walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-100_000_000))
        .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 4) (Ada.lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf 0)
    )
    maliciousExample