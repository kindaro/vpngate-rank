module Main where

import RIO hiding (Handler)
import RIO.Orphans ()
import RIO.Process

import qualified RIO.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy (lines, unlines)
import qualified Data.ByteString as Strict (isPrefixOf)
import qualified Data.ByteString.Char8 as Strict (lines, unlines)
import System.Environment (getArgs)
import Data.Csv
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector
import Path
import Text.Show.Pretty
import Control.Monad.Catch (Handler(..))
import Data.String.Conv

import Control.Sequencer

import VpnGate
import OpenVpn
import qualified Iperf
import Types
import Utils
import Constants
import App

default (Text)

main :: IO ()
main = runApp do
    checkRootOrExit
    -- (iperf, maxSpeed) <- Iperf.choose
    -- entries <- getEntries
    -- rankedEntries <- rankEntries iperf entries
    -- displayBestEntry rankedEntries
    -- let Just entry = listToMaybe entries
    conf <- readFileUtf8 "/tmp/x/vpngate-api-7bc672c815a911b9/openvpn551307-0.ovpn"
    withOpenVpn conf (runProcess_ "tcptraceroute -w 1 -n g.co 443")

getEntries :: (HasProcessContext env, HasLogFunc env) => RIO env [Entry]
getEntries = do
    raw <- getProc "curl" ["--verbose", sourceUrl]
    (parse . clean) raw

  where
    clean :: Lazy.ByteString -> Lazy.ByteString
    clean = Lazy.unlines . filter (not . Lazy.isPrefixOf "*") . Lazy.lines

    parse :: Lazy.ByteString -> RIO env [Entry]
    parse = either (throwM . EncodingException) (pure . toList) . decode @Entry HasHeader

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

makeConfFileLocation :: HasTmpDir env => Entry -> RIO env (Path Abs File)
makeConfFileLocation Entry{..} = do
    hostName' <- (parseRelFile . Text.unpack) hostName
    fileName <- hostName' <.> "ovpn"
    location <- _x fileName
    return location

-- measureOvpn :: HasLogFunc env => Text -> RIO env (Maybe (Domain, Double))
-- measureOvpn conf = withOpenVpn (Conf conf) runIperfs >>= \r -> case r of
--     Left e -> (logWarn . display . Text.pack . ppShow) e >> return Nothing
--     Right (domain, result) -> (return . Just) (domain, getReceivedSpeed result)
