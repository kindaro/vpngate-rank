module Main where

import Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy (lines, unlines)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.ByteString.Base64 as Base64
import System.Environment
import Data.Csv
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word
import Data.Char
import qualified Data.Vector as Vector
import Path
import Path.IO
import Control.FromSum
import Data.List
import Data.Maybe
import Text.Show.Pretty

import VpnGate
import OpenVpn
import Iperf
import Types

main :: IO ()
main = do

    -- Determine the directory to place the vpn configuration files to.
    cwd' <- getEnv "PWD"
    cwd <- parseAbsDir cwd'
    targetDirRelName <- parseRelDir "ovpns"
    let target = cwd </> targetDirRelName

    -- Obtain the source.
    source <- fmap (!! 0) getArgs
    raw <- Lazy.readFile source

    -- Sanitize the source. For some reason, there are these non-standard lines at the beginning
    -- and the end.
    let body = Lazy.unlines . filter (not . Lazy.isPrefixOf "*") . Lazy.lines $ raw

    let rows = (fromEither error . decode @Row HasHeader) body
    putStrLn $ "Done parsing! Number of entries: " ++ show (Vector.length rows)

    -- Write the vpn configuration files.
    createDirIfMissing False target
    paths <- traverse (\x -> fmap (target </>) $ rowToFileName x) rows
    sequence_ $ Vector.zipWith writeRow paths rows

    -- Loop over the configurations, measuring each.
    measurements <- traverse measureOvpn paths

    putStrLn $ "Maximal speed:"
    putStrLn $ ppShow (catMaybes . Vector.toList $ measurements)

rowToFileName :: Row -> IO (Path Rel File)
rowToFileName Row{..} = do
    hostName' <- parseRelFile (Text.unpack hostName :: String)
    fileName <- (hostName' <.> "ovpn" :: IO (Path Rel File))
    return fileName

writeRow :: Path Abs File -> Row -> IO ()
writeRow path Row{..} = do
    let configData = (Text.pack . Char8.unpack . fromEither error . Base64.decode) openVPN_ConfigData_Base64
    Text.writeFile (fromAbsFile path) configData

measureOvpn :: Path Abs File                -- ^ `ovpn` configuration file.
            -> IO (Maybe (Domain, Double))  -- ^ Measure of performance of the respective
                                            --   intermediary.
measureOvpn conf = withOpenVpn conf runIperfs >>= \r -> case r of
    Left error -> print error >> return Nothing
    Right r' -> case r' of
        Left errors -> print errors >> return Nothing
        Right result -> return $ Just (fmap getReceivedSpeed result)
