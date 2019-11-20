module Iperf where

import RIO
import RIO.Orphans ()

import Data.Aeson (eitherDecode)
import System.Process.Typed
import qualified Data.Text as Text
import Control.Sequencer

import JsonIperf
import Types

iperfOptions :: [Text]
iperfOptions =
    [ "--omit", "1"
    , "--interval", "3"
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

data SomethingWrong = SomethingWrong Text deriving Show

instance Exception SomethingWrong where displayException (SomethingWrong x) = "something wrong: " <> show x

reportError :: Show a => a -> RIO env b
reportError = throwM . SomethingWrong . Text.pack . show

runIperf :: HasLogFunc env => Domain -> RIO env TopLevel
runIperf target = do
    logWarn $ "Iperf: Target: " <> displayShow target

    readProcess iperf >>= \(exitCode, out, err) -> case exitCode of
            ExitFailure _ -> reportError err
            ExitSuccess   -> case eitherDecode out of
                Right r  -> return r
                Left err' -> reportError err'
    where iperf = proc "timeout" . fmap Text.unpack $ ["12", "iperf3"] ++ iperfOptions ++ ["--client", domainText target]

runIperfs :: HasLogFunc env => RIO env (Domain, TopLevel)
runIperfs = redundant [defaultHandler] (logWarn . displayShow) (fmap (traverse runIperf . (\x -> (x, x))) servers)
    where fixErrors (errs, out) = maybe (Left errs) Right out
