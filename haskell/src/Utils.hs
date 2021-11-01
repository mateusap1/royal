{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Crypto.Hash
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Ledger ()
import Ledger.Address (Address (..))
import Ledger.Bytes (LedgerBytes (LedgerBytes), fromHex)
import Ledger.Contexts
  ( TxInfo,
    TxOut (..),
    txInInfoResolved,
    txInfoInputs,
  )
import Ledger.Credential (Credential (..))
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Scripts (DatumHash, ValidatorHash)
import Ledger.Value (Value)
import PlutusTx.Prelude
  ( Maybe (..),
    mapMaybe,
    (.),
    (==),
  )
import qualified Prelude as Haskell

-- Return all input outputs located at a specific validator hash
scriptInputsAt :: ValidatorHash -> TxInfo -> [(DatumHash, Value)]
scriptInputsAt vh info = mapMaybe (flt . txInInfoResolved) (txInfoInputs info)
  where
    flt :: TxOut -> Maybe (DatumHash, Value)
    flt
      TxOut
        { txOutDatumHash = Just ds,
          txOutAddress = Address (ScriptCredential s) _,
          txOutValue
        } | s == vh = Just (ds, txOutValue)
    flt _ = Nothing

scriptInputValuesAt :: ValidatorHash -> TxInfo -> [Value]
scriptInputValuesAt vh info = mapMaybe (flt . txInInfoResolved) (txInfoInputs info)
  where
    flt :: TxOut -> Maybe Value
    flt
      TxOut
        { txOutAddress = Address (ScriptCredential s) _,
          txOutValue
        } | s == vh = Just txOutValue
    flt _ = Nothing

pkhFromStr :: Haskell.String -> PubKeyHash
pkhFromStr s =
  case fromHex (pack s) of
    Haskell.Right (LedgerBytes bytes) -> PubKeyHash bytes
    Haskell.Left msg ->
      Haskell.error ("Could not convert from hex to bytes: " Haskell.<> msg)