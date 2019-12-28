module Iperf where

import RIO
import RIO.Orphans ()

import Data.Aeson (eitherDecodeStrict)

import JsonIperf
import Types
import Utils

import RIO.ByteString

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
    ]

choose :: HasLogFunc env => RIO env Url
choose = do
    outputs <- (independent_ . fmap (insistent_ 3 . iperf) . diag @Map) servers
    measurements <- independent_ (fmap decode outputs)
    let speeds :: Map Url Double
        speeds = fmap speed measurements
    case getMaxFromMap speeds of
        [ ]    -> fail "Unable to measure speeds."
        (x: _) -> return x

decode :: ByteString -> RIO env TopLevel
decode x = case eitherDecodeStrict x of
    Left e  -> throwM (EncodingException e)
    Right y -> return y

speed :: TopLevel -> Double
speed x = (sumSentBitsPerSecond . endSumSent . topLevelEnd) x
        + (sumReceivedBitsPerSecond . endSumReceived . topLevelEnd) x

iperf :: HasLogFunc env => Url -> RIO env ByteString
iperf x = getProc "iperf3" (options ++ ["--client", x])

-- runIperf :: HasLogFunc env => Domain -> RIO env TopLevel
--     readProcess iperf >>= \(exitCode, out, err) -> case exitCode of
--             ExitFailure _ -> reportError err
--             ExitSuccess   -> case eitherDecode out of
--                 Right r  -> return r
--                 Left err' -> reportError err'
--     where iperf = proc "timeout" . fmap Text.unpack $ ["12", "iperf3"] ++ options ++ ["--client", domainText target]

-- runIperfs :: HasLogFunc env => RIO env (Domain, TopLevel)
-- runIperfs = redundant [defaultHandler] (logWarn . displayShow) (fmap (traverse runIperf . (\x -> (x, x))) servers)
--     where fixErrors (errs, out) = maybe (Left errs) Right out
