module Main where

import RIO hiding (Handler)
import RIO.Orphans ()

import qualified RIO.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy (lines, unlines)
import qualified Data.ByteString.Base64 as Base64
import System.Environment (getArgs)
import Data.Csv
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector
import Path
import Text.Show.Pretty
import Control.Monad.Catch (Handler(..))

import Control.Sequencer

import VpnGate
import OpenVpn
import Iperf
import Types

default (Text)

main = runSimpleApp do
    iperf <- chooseIperf
    entries <- getEntries
    rankedEntries <- rankEntries iperf entries
    displayBestEntry rankedEntries

sourceUrl :: Text
sourceUrl = "https://www.vpngate.net/api/iphone/"

chooseIperf :: RIO env Text
chooseIperf = _u

getEntries :: RIO env [Entry]
getEntries = _u

rankEntries :: Text -> [Entry] -> RIO env [(Entry, Meta)]
rankEntries = _u

displayBestEntry :: [(Entry, Meta)] -> RIO env ()
displayBestEntry = _u

    -- -- Obtain the source.
    -- source <- liftIO getArgs >>= \x -> case listToMaybe x of
    --     Nothing -> throwM . Error $ "Expected one argument."
    --     Just y  -> return y
    -- raw <- Lazy.readFile source

    -- -- Sanitize the source. For some reason, there are these non-standard lines at the beginning
    -- -- and the end.
    -- let body = Lazy.unlines . filter (not . Lazy.isPrefixOf "*") . Lazy.lines $ raw

    -- rows <- either (throwM . Error) pure $ decode @Row HasHeader body
    -- logWarn $ "Done parsing! Number of entries: " <> display (Vector.length rows)

    -- -- Loop over the configurations, measuring each.
    -- measurements <- independent @_ @_ @_ @_ @Vector [Handler \e -> return (e :: SomeException)] (logWarn . displayShow) $ fmap (obtainConf >=> measureOvpn) rows

    -- logWarn "Maximal speed:"
    -- logWarn . display . Text.pack . ppShow . catMaybes . Vector.toList $ measurements

getConf :: Entry -> RIO env Text
getConf = fmap decodeUtf8Lenient . either (throwM . EncodingException) pure
        . Base64.decode . openVPN_ConfigData_Base64

makeConfFileLocation :: HasTmpDir env => Entry -> RIO env (Path Abs File)
makeConfFileLocation Entry{..} = do
    hostName' <- (parseRelFile . Text.unpack) hostName
    fileName <- hostName' <.> "ovpn"
    location <- _x fileName
    return location

measureOvpn :: HasLogFunc env => Text -> RIO env (Maybe (Domain, Double))
measureOvpn conf = withOpenVpn (Conf conf) runIperfs >>= \r -> case r of
    Left e -> (logWarn . display . Text.pack . ppShow) e >> return Nothing
    Right (domain, result) -> (return . Just) (domain, getReceivedSpeed result)
