module VpnGate where

import Data.Csv
import Data.Text (Text)
import qualified Data.ByteString as Strict
import GHC.Generics

data Row = Row
    { hostName
    , ip
    , score
    , ping
    , speed
    , countryLong
    , countryShort
    , numVpnSessions
    , uptime
    , totalUsers
    , totalTraffic
    , logType
    , operator
    , message :: Text
    , openVPN_ConfigData_Base64 :: Strict.ByteString
    } deriving (Generic, Show)

instance FromRecord Row
instance ToRecord Row
