module Main where

import           RIO
import qualified RIO.ByteString.Lazy        as Lazy
import           RIO.Map                    (toDescList)
import           RIO.Orphans                ()
import           RIO.Process

import           Data.Bifunctor
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
    logInfo "Measuring iperf servers..."
    (iperfUrl, maxSpeed) <- chooseIperf
    logInfo $ "Selected iperf server " <> displayShow iperfUrl
               <> " with speed " <> display (showAsMbps maxSpeed) <> "."
    logInfo "Loading VPN entries..."
    entries <- getEntries
    logInfo $ "Loaded " <> displayShow (length entries) <> " VPN entries."
    logInfo "Ranking entries..."
    rankedEntries <- rankEntries iperfUrl entries
    traverse_ (logInfo . displayShow . bimap entry_hostName showAsMbps) rankedEntries
    case rankedEntries of
        [ ] -> logWarn "Unfortunately, no VPNs responded."
        ((entry, _): _) -> do
            logInfo $ "The fastest VPN entry: " <> displayShow entry
            conf <- getConf entry
            message conf

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
getSpeedOverVpn iperfUrl openVpnConf = cool_ 1 3 do
    -- I allot 10 seconds to initialization, while another 10 seconds are taken by the iperf run.
    x <- timeout (20 * 10^(6 :: Int))
            $ withOpenVpnConf openVpnConf $ cool_ 1 3 (measureSpeed iperfUrl)
    maybe (throwM TimeoutException) return $ x
