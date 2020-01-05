module Main where

import RIO
import RIO.Orphans ()
import RIO.Process
import RIO.Map (toDescList)

import qualified RIO.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy (lines, unlines)
import qualified Data.ByteString as Strict (isPrefixOf)
import qualified Data.ByteString.Char8 as Strict (lines, unlines)
import System.Environment (getArgs)
import Data.Csv (HasHeader(..))
import qualified Data.Csv as Csv
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector
import Path
import Text.Show.Pretty
import Data.String.Conv

import Control.Sequencer

import VpnGate (Entry, getConf)
import OpenVpn
import Iperf (getSpeed)
import qualified Iperf as Iperf
import Types
import Utils
import Constants
import App

default (Text)

main :: IO ()
main = runApp do
    checkRootOrExit
    (iperf, maxSpeed) <- Iperf.choose
    entries <- getEntries
    rankedEntries <- rankEntries iperf entries
    message $ "Max iperf speed: " <> tshow maxSpeed
    traverse_ (message . tshow) rankedEntries

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
    measuredEntries <- traverse (getConf >=> getSpeedOverVpn iperfUrl) (diag @Map entries)
    let sortedEntries = (unfibers . toDescList . fibers) measuredEntries
    return sortedEntries

    -- rows <- either (throwM . Error) pure $ decode @Row HasHeader body
    -- logWarn $ "Done parsing! Number of entries: " <> display (Vector.length rows)

    -- -- Loop over the configurations, measuring each.
    -- measurements <- independent @_ @_ @_ @_ @Vector [Handler \e -> return (e :: SomeException)] (logWarn . displayShow) $ fmap (obtainConf >=> measureOvpn) rows

    -- logWarn "Maximal speed:"
    -- logWarn . display . Text.pack . ppShow . catMaybes . Vector.toList $ measurements

getSpeedOverVpn
    :: (HasProcessContext env, HasLogFunc env, HasTmpDir env)
    => Url -> Text -> RIO env Double
getSpeedOverVpn iperfUrl openVpnConf = withOpenVpnConf openVpnConf (getSpeed iperfUrl)

-- measureOvpn :: HasLogFunc env => Text -> RIO env (Maybe (Domain, Double))
-- measureOvpn conf = withOpenVpn (Conf conf) runIperfs >>= \r -> case r of
--     Left e -> (logWarn . display . Text.pack . ppShow) e >> return Nothing
--     Right (domain, result) -> (return . Just) (domain, getReceivedSpeed result)
