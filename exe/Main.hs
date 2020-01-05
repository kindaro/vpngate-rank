module Main where

import           RIO
import qualified RIO.ByteString.Lazy        as Lazy
import           RIO.Map                    (toDescList)
import           RIO.Orphans                ()
import           RIO.Process

import qualified Data.ByteString.Lazy.Char8 as Lazy (lines, unlines)
import           Data.Csv                   (HasHeader (..))
import qualified Data.Csv                   as Csv

import           App
import           Constants
import           Iperf
import           OpenVpn
import           Types
import           Utils
import           VpnGate

default (Text)

main :: IO ()
main = runApp do
    checkRootOrExit
    (iperf, maxSpeed) <- chooseIperf
    logInfo $ "Selected iperf server " <> displayShow iperf
               <> " with speed " <> display (showAsMbps maxSpeed) <> "."
    entries <- getEntries
    logInfo $ "Loaded " <> displayShow (length entries) <> " VPN entries."
    rankedEntries <- rankEntries iperf entries
    traverse_ (logInfo . displayShow) rankedEntries
    case rankedEntries of
        [ ] -> logWarn "Unfortunately, no VPNs responded."
        ((entry, _): _) -> do
            logInfo $ "The fastest VPN entry: " <> displayShow entry
            conf <- getConf entry
            message $ "Configuration:\n\n" <> conf


getEntries :: (HasProcessContext env, HasLogFunc env) => RIO env [Entry]
getEntries = do
    raw <- getProc "curl" ["--verbose", sourceUrl]
    (parse . clean) raw

  where
    parse :: Lazy.ByteString -> RIO env [Entry]
    parse = either (throwM . EncodingException) (pure . toList) . Csv.decode @Entry HasHeader

    clean :: Lazy.ByteString -> Lazy.ByteString
    clean = Lazy.unlines . filter (not . Lazy.isPrefixOf "*") . Lazy.lines

rankEntries
    :: (HasProcessContext env, HasLogFunc env, HasTmpDir env)
    => Url -> [Entry] -> RIO env [(Entry, Double)]
rankEntries iperfUrl entries = do
    measuredEntries <- independent_ $ fmap (getConf >=> getSpeedOverVpn iperfUrl)
                                    $ diag @Map entries
    let sortedEntries = (unfibers . toDescList . fibers) measuredEntries
    return sortedEntries

getSpeedOverVpn
    :: (HasProcessContext env, HasLogFunc env, HasTmpDir env)
    => Url -> Text -> RIO env Double
getSpeedOverVpn iperfUrl openVpnConf = cool_ 1 10
                                     $ withOpenVpnConf openVpnConf
                                     $ measureSpeed iperfUrl
