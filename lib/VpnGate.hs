module VpnGate
    ( Entry
    , getConf
    )
    where

import RIO

import qualified Data.ByteString.Base64 as Base64
import           Data.Csv (FromRecord, ToRecord)

-- import JsonIperf ()
import Types

data Entry = Entry
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
    , openVPN_ConfigData_Base64 :: ByteString
    } deriving (Generic, Show, Eq, Ord)

instance FromRecord Entry
instance ToRecord Entry

getConf :: VpnGate.Entry -> RIO env Text
getConf = fmap decodeUtf8Lenient . either (throwM . EncodingException) pure
        . Base64.decode . VpnGate.openVPN_ConfigData_Base64
