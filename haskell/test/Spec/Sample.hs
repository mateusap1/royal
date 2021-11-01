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

module Spec.Sample where

import Distributor
import PlutusTx.Prelude (Integer)
import Ledger.Crypto (PubKeyHash, pubKeyHash)
import Wallet.Emulator.Wallet (walletPubKey, knownWallet)
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.List (zip)

{-# INLINABLE pubKeyHashes #-}
pubKeyHashes :: [Integer] -> [PubKeyHash]
pubKeyHashes pkhs = [pubKeyHash (walletPubKey (knownWallet w)) | w <- pkhs]

{-# INLINABLE sampleDistributionSettings #-}
sampleDistributionSettings :: DistributionSettings
sampleDistributionSettings =  DistributionSettings
  { dsMinUtxoValue = minUtxoValue,
    dsDistributionMapping = 
      PlutusMap.fromList (zip (pubKeyHashes [2 .. 5]) [10, 10, 40, 40])
  }