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

module Spec.Run where

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
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
import Spec.Example
import Spec.Sample
import Test.Tasty ()
import Prelude (IO, Show (..), String)
import qualified Prelude

runMyExample :: IO ()
runMyExample =
  runEmulatorTraceIO' def (abstractConfig [1 .. 3]) myExample

runSimpleExample :: IO ()
runSimpleExample =
  runEmulatorTraceIO' def (abstractConfig [1 .. 5]) simpleExample

runMultipleUtxosTokensExample :: IO ()
runMultipleUtxosTokensExample =
  runEmulatorTraceIO' def (abstractConfig [1 .. 7]) multipleUtxosExample

runBelowMinimumExample :: IO ()
runBelowMinimumExample =
  runEmulatorTraceIO' def (abstractConfig [1 .. 5]) belowMinimumExample

runMaliciousExample :: IO ()
runMaliciousExample =
  runEmulatorTraceIO' def (abstractConfig [1 .. 5]) maliciousExample

abstractConfig :: [Integer] -> EmulatorConfig
abstractConfig ns =
  EmulatorConfig
    (Left $ Map.fromList [(knownWallet w, setValue w) | w <- ns])
    def
    def
  where
    setValue :: Integer -> Value
    setValue _ =
      Ada.lovelaceValueOf 1_000_000_000