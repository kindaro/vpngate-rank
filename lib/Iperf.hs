module Iperf where

import RIO
import RIO.Orphans ()

import Data.Aeson (eitherDecode)
import System.Process.Typed
import qualified Data.Text as Text
import Control.Sequencer

import JsonIperf
import Types
import Utils

iperfOptions :: [Text]
iperfOptions =
    [ "--omit", "1"
    , "--interval", "3"
    , "--connect-timeout", "1000"
    , "--json"
    ]

servers :: [Url]
servers =
    [ "bouygues.iperf.fr"
    , "ping.online.net"
    , "ping6.online.net"
    , "ping-90ms.online.net"
    , "ping6-90ms.online.net"
    , "speedtest.serverius.net"
    , "iperf.eenet.ee"
    , "iperf.volia.net"
    , "iperf.it-north.net"
    , "iperf.biznetnetworks.com"
    , "iperf.scottlinux.com"
    , "iperf.he.net"
    ]

chooseIperf :: HasLogFunc env => RIO env Url
chooseIperf = do
    measurements <-
        let iperfs :: HasLogFunc env => Map Url (RIO env TopLevel)
            iperfs = (fmap makeIperfProcess . makeDiagMap) servers
        in independent _handlers _logger iperfs
    return _u

makeIperfProcess :: HasLogFunc env => Url -> RIO env TopLevel
makeIperfProcess = (insistent _handlers _logger 3 . makeIperfProcess)

-- runIperf :: HasLogFunc env => Domain -> RIO env TopLevel
--     readProcess iperf >>= \(exitCode, out, err) -> case exitCode of
--             ExitFailure _ -> reportError err
--             ExitSuccess   -> case eitherDecode out of
--                 Right r  -> return r
--                 Left err' -> reportError err'
--     where iperf = proc "timeout" . fmap Text.unpack $ ["12", "iperf3"] ++ iperfOptions ++ ["--client", domainText target]

-- runIperfs :: HasLogFunc env => RIO env (Domain, TopLevel)
-- runIperfs = redundant [defaultHandler] (logWarn . displayShow) (fmap (traverse runIperf . (\x -> (x, x))) servers)
--     where fixErrors (errs, out) = maybe (Left errs) Right out
