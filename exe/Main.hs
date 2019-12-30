module Main where

import RIO hiding (Handler)
import RIO.Orphans ()

import qualified RIO.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy (lines, unlines)
import qualified Data.ByteString as Strict (isPrefixOf)
import qualified Data.ByteString.Char8 as Strict (lines, unlines)
import qualified Data.ByteString.Base64 as Base64
import System.Environment (getArgs)
import Data.Csv
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector
import Path
import Text.Show.Pretty
import Control.Monad.Catch (Handler(..))

import Control.Sequencer

import qualified VpnGate
import OpenVpn
import qualified Iperf
import Types
import Utils

default (Text)

main = runApp do
    (iperf, maxSpeed) <- Iperf.choose
    entries <- getEntries
    -- rankedEntries <- rankEntries iperf entries
    -- displayBestEntry rankedEntries
    message $ (tshow . take 1 . reverse) entries

sourceUrl :: Url
sourceUrl = "https://www.vpngate.net/api/iphone/"

getEntries :: HasLogFunc env => RIO env [VpnGate.Entry]
getEntries = do
    raw <- getProc "curl" [sourceUrl]
    (parse . clean) raw

  where
    clean :: Lazy.ByteString -> Lazy.ByteString
    clean = Lazy.unlines . filter (not . Lazy.isPrefixOf "*") . Lazy.lines

    parse :: Lazy.ByteString -> RIO env [VpnGate.Entry]
    parse = either (throwM . EncodingException) (pure . toList) . decode @VpnGate.Entry HasHeader

rankEntries :: Text -> [VpnGate.Entry] -> RIO env [(VpnGate.Entry, Meta)]
rankEntries = _u

displayBestEntry :: [(VpnGate.Entry, Meta)] -> RIO env ()
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

getConf :: VpnGate.Entry -> RIO env Text
getConf = fmap decodeUtf8Lenient . either (throwM . EncodingException) pure
        . Base64.decode . VpnGate.openVPN_ConfigData_Base64

makeConfFileLocation :: HasTmpDir env => VpnGate.Entry -> RIO env (Path Abs File)
makeConfFileLocation VpnGate.Entry{..} = do
    hostName' <- (parseRelFile . Text.unpack) hostName
    fileName <- hostName' <.> "ovpn"
    location <- _x fileName
    return location

-- measureOvpn :: HasLogFunc env => Text -> RIO env (Maybe (Domain, Double))
-- measureOvpn conf = withOpenVpn (Conf conf) runIperfs >>= \r -> case r of
--     Left e -> (logWarn . display . Text.pack . ppShow) e >> return Nothing
--     Right (domain, result) -> (return . Just) (domain, getReceivedSpeed result)
