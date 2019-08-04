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

import VpnGate
import OpenVpn
import Iperf

main :: IO ()
main = do

    cwd' <- getEnv "PWD"
    cwd <- parseAbsDir cwd'
    targetDirRelName <- parseRelDir "ovpns"
    let target = cwd </> targetDirRelName
    source <- fmap (!! 0) getArgs
    raw <- Lazy.readFile source
    let body = Lazy.unlines . filter (not . Lazy.isPrefixOf "*") . Lazy.lines $ raw
        -- For some reason, there are these non-standard lines at the beginning and the end.
    let parsed = (fromEither error . decode @Row HasHeader) body
    putStrLn $ "Done parsing! Number of entries: " ++ show (Vector.length parsed)
    createDirIfMissing False target
    let actions = fmap (writeOvpn target) parsed
    sequence_ actions

writeOvpn :: Path Abs Dir -> Row -> IO ()
writeOvpn path Row{..} = do
    hostName' <- (parseRelFile . Text.unpack) hostName
    filename <- hostName' <.> "ovpn"
    let configData = (Text.pack . Char8.unpack . fromEither error . Base64.decode) openVPN_ConfigData_Base64
    Text.writeFile (fromAbsFile $ path </> filename) configData

measureOvpn :: Path Abs File  -- ^ `ovpn` configuration file.
            -> IO Int         -- ^ Measure of performance of the respective intermediary.

    -- I need to interact with the `openvpn` process and expect the line like "Initialization
    -- Sequence Completed", and then any other line will signal error and I should terminate. A
    -- well-behaved run of `openvpn` must initialize and let me do the work.

measureOvpn conf = do
    undefined
    -- launch ovpn
    -- expect the good line
    -- launch iperf insistently
    -- capture value of the first iperf that succeeds
    -- return it
