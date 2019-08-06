module Iperf where

import RIO

import Data.Aeson (eitherDecode)
import System.Process.Typed
import Data.Text (Text)
import qualified Data.Text as Text

import JsonIperf
import Types

iperfOptions :: [Text]
iperfOptions =
    [ "--omit", "1"
    , "--interval", "10"
    , "--connect-timeout", "1000"
    , "--json"
    ]

servers :: [Domain]
servers =
    [ Domain "bouygues.iperf.fr"
    , Domain "ping.online.net"
    , Domain "ping6.online.net"
    , Domain "ping-90ms.online.net"
    , Domain "ping6-90ms.online.net"
    , Domain "speedtest.serverius.net"
    , Domain "iperf.eenet.ee"
    , Domain "iperf.volia.net"
    , Domain "iperf.it-north.net"
    , Domain "iperf.biznetnetworks.com"
    , Domain "iperf.scottlinux.com"
    , Domain "iperf.he.net"
    ]

runIperf :: Domain -> IO (Either Error TopLevel)
runIperf target = readProcess iperf >>= \(exitCode, out, err) -> case exitCode of
            ExitFailure _ -> return (Left . Error $ err)
            ExitSuccess   -> case eitherDecode out of
                Right r  -> return (Right r)
                Left err' -> return (Left . Error $ err')
    where iperf = proc "iperf3" . fmap Text.unpack $ iperfOptions ++ ["--client", domainText target]

runUntilHappy :: (a -> IO (Either Error b)) -> [a] -> IO ([(a, Error)], Maybe (a, b))
runUntilHappy _       [ ]     = return ([ ], Nothing)
runUntilHappy program (target: targets) = program target >>= \r' -> case r' of
        Right r -> return ([ ], Just (target, r))
        Left  e -> runUntilHappy program targets >>= \(errs, x) -> return ((target, e): errs, x)

runIperfs :: IO (Either [(Domain, Error)] (Domain, TopLevel))
runIperfs = fmap fixErrors (runUntilHappy runIperf servers)
    where fixErrors (errs, out) = maybe (Left errs) Right out
