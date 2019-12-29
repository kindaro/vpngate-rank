module Iperf where

import RIO
import RIO.Orphans ()
import Data.Aeson (eitherDecodeStrict)
import Data.Map.Strict ((!))

import JsonIperf
import Types
import Utils

options :: [ByteString]
options =
    [ "--omit", "1"
    , "--interval", "3"
    , "--connect-timeout", "1000"
    , "--json"
    ]

servers :: [Url]
servers =
    [ "bouygues.iperf.fr"
    , "iperf.he.net"
    , "speedtest.wtnet.de"
    ]

choose :: HasLogFunc env => RIO env (Url, Double)
choose = do
    outputs <- (independent_ . fmap (cool_ delay 3 . iperf) . diag @Map) servers
    measurements <- independent_ (fmap decode outputs)
    let speeds :: Map Url Double
        speeds = fmap speed measurements
    case getMaxFromMap speeds of
        [ ]    -> fail "Unable to measure speeds."
        (x: _) -> return (x, speeds ! x)

  where
    delay = 10 ^ (6 :: Int)

decode :: ByteString -> RIO env TopLevel
decode x = case eitherDecodeStrict x of
    Left e  -> throwM (EncodingException e)
    Right y -> return y

speed :: TopLevel -> Double
speed x = getReceivedSpeed x

iperf :: HasLogFunc env => Url -> RIO env ByteString
iperf x = getProc "iperf3" (options ++ ["--client", x])

getSentSpeed, getReceivedSpeed :: TopLevel -> Double
getSentSpeed     = sumSentBitsPerSecond     . endSumSent     . topLevelEnd
getReceivedSpeed = sumReceivedBitsPerSecond . endSumReceived . topLevelEnd
