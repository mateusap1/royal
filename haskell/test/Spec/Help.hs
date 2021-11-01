module Spec.Help where

-- import PlutusTx (toData)
-- import Data.Aeson

import Cardano.Api
import Cardano.Api.Shelley
import Data.Aeson (encode)
import Data.ByteString.Lazy
import qualified Data.ByteString.Short as SBS
import Distributor (DistributionRedeemer)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude as P (BuiltinByteString)
import System.Environment
import Prelude

redeemerJSON :: DistributionRedeemer -> ByteString
redeemerJSON dr =
  encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData dr))
