module Iperf where

import RIO

import Data.Aeson (eitherDecode)
import System.Process.Typed
import qualified Data.Text as Text
import Control.Sequencer

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

runIperfs :: IO (Domain, Either Error TopLevel)
runIperfs = redundant_ (fmap (traverse runIperf . (\x -> (x, x))) servers)
    where fixErrors (errs, out) = maybe (Left errs) Right out
