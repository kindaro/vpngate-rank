module VpnGate where

import RIO

import qualified Data.ByteString.Base64 as Base64
import           Data.Csv (FromRecord, ToRecord)

-- import JsonIperf ()
import Types

data Entry = Entry
    { entry_hostName
    , entry_ip
    , entry_score
    , entry_ping
    , entry_speed
    , entry_countryLong
    , entry_countryShort
    , entry_numVpnSessions
    , entry_uptime
    , entry_totalUsers
    , entry_totalTraffic
    , entry_logType
    , entry_operator
    , entry_message :: Text
    , entry_openVPN_ConfigData_Base64 :: ByteString
    } deriving (Generic, Show, Eq, Ord)

instance FromRecord Entry
instance ToRecord Entry

getConf :: VpnGate.Entry -> RIO env Text
getConf = fmap decodeUtf8Lenient . either (throwM . EncodingException) pure
        . Base64.decode . entry_openVPN_ConfigData_Base64
