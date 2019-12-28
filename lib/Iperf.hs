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

iperfOptions :: [ByteString]
iperfOptions =
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

-- chooseIperf :: HasLogFunc env => RIO env Url
-- chooseIperf = do
--     measurements <-
--         let iperfs :: HasLogFunc env => Map Url (RIO env TopLevel)
--             iperfs = (fmap makeIperfProcess . makeDiagMap) servers
--         in independent _handlers _logger iperfs
--     return _u

iperf :: HasLogFunc env => Url -> RIO env ByteString
iperf x = getProc "iperf3" (iperfOptions ++ ["--client", x])

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
