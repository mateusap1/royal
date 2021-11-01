{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import Cardano.Api hiding (Value)
import Cardano.Api.Shelley hiding (Value)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Codec.Serialise
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Either (fromRight)
import Data.String (IsString (fromString))
import Distributor
import Distributor.OnChain
import Utils
import Ledger (PubKeyHash (..), pubKeyHash)
import Ledger.Bytes (LedgerBytes (LedgerBytes), fromHex)
import Ledger.Crypto (PubKeyHash, pubKeyHash)
import Ledger.Value (Value)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as PlutusMap
import System.Environment
import System.IO
import Prelude

scriptsPath :: [Char]
scriptsPath = "../scripts"

-- Our script
distributionScript :: DistributionSettings -> Plutus.Script
distributionScript = Plutus.unValidatorScript . distributionValidator

-- A short bytestring version of our script to print in the terminal
distributionSBS :: DistributionSettings -> SBS.ShortByteString
distributionSBS = SBS.toShort . LBS.toStrict . serialise . distributionScript

-- Our script serialised
distributionSerialised :: DistributionSettings -> PlutusScript PlutusScriptV1
distributionSerialised = PlutusScriptSerialised . distributionSBS

jsonCompile :: IO ()
jsonCompile = do
  -- Get shell arguments
  args <- getArgs

  let numOfArgs :: Int
      numOfArgs = length args

      filePath :: FilePath
      filePath = if numOfArgs > 0 then head args else "data/ds.json"

  -- The distribution settings in a json format
  dsBS <- LBS.readFile filePath

  let m :: Maybe DistributionSettings
      m = Aeson.decode dsBS :: Maybe DistributionSettings

  case m of
    Nothing -> error "Couldn't distribution settings"
    Just ds -> do
      putStrLn $ "Writing output to: " ++ scriptName

      writePlutusScript () (scriptsPath ++ "/" ++ scriptName) scriptSerial scriptSBS
      where
        scriptName :: String
        scriptName = if numOfArgs > 1 then args !! 1 else "result.plutus"

        scriptSerial :: PlutusScript PlutusScriptV1
        scriptSerial = distributionSerialised ds

        scriptSBS :: SBS.ShortByteString
        scriptSBS = distributionSBS ds

readPubKeyHashes :: Integer -> IO [PubKeyHash]
readPubKeyHashes i = do
  putStr ("Public Key Hash " ++ show i ++ " (ENTER to skip): ")
  hFlush stdout
  pkh <- getLine
  case pkh of
    "" -> do
      return []
    _ -> do
      nextPKHs <- readPubKeyHashes (i + 1)
      return $ pkhFromStr pkh : nextPKHs

-- (pure (PlutusMap.empty :: PlutusMap.Map PubKeyHash Integer))
readDistribution :: [PubKeyHash] -> IO (PlutusMap.Map PubKeyHash Integer)
readDistribution pkhs = do
  foldr f (pure (PlutusMap.empty :: PlutusMap.Map PubKeyHash Integer)) pkhs
  where
    f ::
      PubKeyHash ->
      IO (PlutusMap.Map PubKeyHash Integer) ->
      IO (PlutusMap.Map PubKeyHash Integer)
    f pkh acc = do
      putStr $ (take 20 . show) pkh ++ "... weight: "
      hFlush stdout

      weight <- getLine

      PlutusMap.insert pkh (read weight :: Integer) <$> acc

readMinUtxoValue :: IO Value
readMinUtxoValue = do
  putStr "Minimum UTxO value: "
  hFlush stdout
  min <- getLine
  return $ lovelaceValueOf (read min :: Integer)

inputCompile :: IO ()
inputCompile = do
  -- Get shell arguments
  args <- getArgs

  let numOfArgs :: Int
      numOfArgs = length args

  pkhs <- readPubKeyHashes 1

  dsMap <- readDistribution pkhs

  minUtxoValue <- readMinUtxoValue

  let ds :: DistributionSettings
      ds =
        DistributionSettings
          { dsMinUtxoValue = minUtxoValue,
            dsDistributionMapping = dsMap
          }

      scriptName :: String
      scriptName = if numOfArgs > 0 then head args else "result.plutus"

      scriptSerial :: PlutusScript PlutusScriptV1
      scriptSerial = distributionSerialised ds

      scriptSBS :: SBS.ShortByteString
      scriptSBS = distributionSBS ds

  putStrLn $ "Writing output to: " ++ scriptName
  writePlutusScript () (scriptsPath ++ "/" ++ scriptName) scriptSerial scriptSBS

main :: IO ()
main = inputCompile

writePlutusScript :: () -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript datum filename scriptSerial scriptSBS =
  do
    case Plutus.defaultCostModelParams of
      Just m ->
        let pData = Plutus.toData datum
            (logout, e) =
              Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
         in do
              print ("Log output" :: String) >> print logout
              case e of
                Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
      Nothing -> error "defaultCostModelParams failed"
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()