module Main where

import RIO
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

import Control.Sequencer

import VpnGate
import OpenVpn
import Iperf
import Types

default (Text)

main :: IO ()
main = runSimpleApp do

    -- Obtain the source.
    source <- liftIO getArgs >>= \x -> case listToMaybe x of
        Nothing -> throwM . Error $ "Expected one argument."
        Just y  -> return y
    raw <- Lazy.readFile source

    -- Sanitize the source. For some reason, there are these non-standard lines at the beginning
    -- and the end.
    let body = Lazy.unlines . filter (not . Lazy.isPrefixOf "*") . Lazy.lines $ raw

    rows <- either (throwM . Error) pure $ decode @Row HasHeader body
    logWarn $ "Done parsing! Number of entries: " <> display (Vector.length rows)

    -- Loop over the configurations, measuring each.
    measurements <- independent_ @_ @_ @Vector $ fmap (obtainConf >=> measureOvpn) rows

    logWarn "Maximal speed:"
    logWarn . display . Text.pack . ppShow . catMaybes . Vector.toList $ measurements

obtainConf :: Row -> RIO env Text
obtainConf = fmap decodeUtf8Lenient . either (throwM . Error) pure . Base64.decode . openVPN_ConfigData_Base64

rowToFileName :: Row -> IO (Path Rel File)
rowToFileName Row{..} = do
    hostName' <- parseRelFile (Text.unpack hostName :: String)
    fileName <- (hostName' <.> "ovpn" :: IO (Path Rel File))
    return fileName

measureOvpn :: HasLogFunc env => Text -> RIO env (Maybe (Domain, Double))
measureOvpn conf = withOpenVpn (Conf conf) (liftIO runIperfs) >>= \r -> case r of
    Left e -> (logWarn . display . Text.pack . ppShow) e >> return Nothing
    Right (domain, r') -> case r' of
        Left es -> (logWarn . display . Text.pack . ppShow) es >> return Nothing
        Right result -> return $ Just (domain, getReceivedSpeed result)
