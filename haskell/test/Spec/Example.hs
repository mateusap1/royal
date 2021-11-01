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

module Spec.Example where

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..), (<>))
import Data.Text (Text, pack)
import Distributor
import Distributor.OffChain
import Ledger
import Ledger.Ada as Ada (fromValue, getLovelace, lovelaceValueOf)
import Ledger.TimeSlot
import Ledger.Value (AssetClass (AssetClass), assetClass, assetClassValue)
import Plutus.Contract.Test (Wallet (Wallet), knownWallet, walletPubKey)
import Plutus.Contract.Trace (Wallet (Wallet))
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
import PlutusTx.Monoid (Monoid (mempty))
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinByteString,
    Either (Left),
    Integer,
    Maybe (Just, Nothing),
    return,
    zip,
    ($),
    (*),
    (++),
    (-),
    (<),
  )
import qualified PlutusTx.Ratio as R
import Spec.Sample
import Spec.Trace
import Test.Tasty ()
import Prelude (IO, Show (..), String)
import qualified Prelude

-- Repeat the given emulator trace action N times
repeatTrace :: EmulatorTrace () -> Integer -> EmulatorTrace ()
repeatTrace _ 0 = return ()
repeatTrace t n
  | n < 0 = Prelude.error "Repeat times cannot be negative"
  | Prelude.otherwise = do
    t
    repeatTrace t (n - 1)

myExample :: EmulatorTrace ()
myExample = do
  let alice, bob, charlie :: Wallet
      alice = knownWallet 1
      bob = knownWallet 2
      charlie = knownWallet 3

      ds :: DistributionSettings
      ds =
        DistributionSettings
          { dsMinUtxoValue = minUtxoValue,
            dsDistributionMapping =
              PlutusMap.fromList (zip (pubKeyHashes [1 .. 3]) [33, 33, 33])
          }

  aliceHandler <- activateContractWallet alice distributorEndpoints
  -- bobHandler <- activateContractWallet bob distributorEndpoints
  -- charlieHandler <- activateContractWallet charlie distributorEndpoints

  distributorSendTrace aliceHandler ds 9_000_000

  void $ Emulator.waitNSlots 1

  distributeTrace aliceHandler sampleDistributionSettings

  void $ Emulator.waitNSlots 1

-- The following traces should succeed
simpleExample :: EmulatorTrace ()
simpleExample = do
  let alice, bob :: Wallet
      alice = knownWallet 1 -- The user who will donate the 100 ADA to the distribution script
      bob = knownWallet 2 -- One of the receivers who will distribute the locked ADA
  aliceHandler <- activateContractWallet alice distributorEndpoints

  distributorSendTrace aliceHandler sampleDistributionSettings 100_000_000

  void $ Emulator.waitNSlots 1

  bobHandler <- activateContractWallet bob distributorEndpoints

  distributeTrace bobHandler sampleDistributionSettings

  void $ Emulator.waitNSlots 1

multipleUtxosExample :: EmulatorTrace ()
multipleUtxosExample = do
  let alice, bob, random :: Wallet
      alice = knownWallet 1
      bob = knownWallet 2
      random = knownWallet 7

  aliceHandler <- activateContractWallet alice distributorEndpoints
  randomHandler <- activateContractWallet random distributorEndpoints

  let sendTokens :: EmulatorTrace ()
      sendTokens = do
        distributorSendTrace aliceHandler sampleDistributionSettings 10_000_000

        distributorSendTrace randomHandler sampleDistributionSettings 10_000_000

        void $ Emulator.waitNSlots 1

  repeatTrace sendTokens 5

  bobHandler <- activateContractWallet bob distributorEndpoints

  distributeTrace bobHandler sampleDistributionSettings

  void $ Emulator.waitNSlots 1

-- The following traces should fail
belowMinimumExample :: EmulatorTrace ()
belowMinimumExample = do
  let alice, bob :: Wallet
      alice = knownWallet 1 -- The user who will donate the 100 ADA to the distribution script
      bob = knownWallet 2 -- One of the receivers who will distribute the locked ADA
  aliceHandler <- activateContractWallet alice distributorEndpoints

  distributorSendTrace
    aliceHandler
    sampleDistributionSettings
    (getLovelace (fromValue minUtxoValue) * 10 - 100)

  void $ Emulator.waitNSlots 1

  bobHandler <- activateContractWallet bob distributorEndpoints

  distributeTrace bobHandler sampleDistributionSettings

  void $ Emulator.waitNSlots 1

maliciousExample :: EmulatorTrace ()
maliciousExample = do
  let alice, bob :: Wallet
      alice = knownWallet 1
      bob = knownWallet 2
  aliceHandler <- activateContractWallet alice distributorEndpoints

  distributorSendTrace aliceHandler sampleDistributionSettings 100_000_000

  void $ Emulator.waitNSlots 1

  bobHandler <- activateContractWallet bob distributorEndpoints

  let -- Instead of correctly distributing the script ADA, we'll keep everything
      mlcDis :: PlutusMap.Map PubKeyHash Integer
      mlcDis = PlutusMap.fromList [(pubKeyHash (walletPubKey bob), 1)]

  maliciousDistributeTrace bobHandler sampleDistributionSettings mlcDis

  void $ Emulator.waitNSlots 1