module OpenVpn where

import RIO hiding (withSystemTempFile)

import System.Process.Typed
import Path
import Path.IO
import Text.Megaparsec
import Text.Megaparsec.Byte
import Control.Monad.Combinators
import Data.Void
import Control.Monad.Extra
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Either
import System.IO.Error
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.UTF8 as StrictUTF8
import Data.Char
import System.Process (interruptProcessGroupOf)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Catch (MonadMask)
import System.IO (SeekMode(..))

import Types
import Iperf
import JsonIperf

newtype Conf = Conf { conf :: Text } deriving (Show, Eq, Ord)

-- withSystemTempFileTyped :: (MonadIO m, MonadMask m)
--                         => Strict.ByteString -> (Path Abs File -> Handle -> m a) -> m a
-- withSystemTempFileTyped pattern action = withSystemTempFile (StrictUTF8.toString pattern) $ \name' handle -> do
--     name <- parseAbsFile name'
--     action name handle

withOpenVpn :: Conf -> IO a -> IO (Either Error a)
withOpenVpn Conf{..} io = withSystemTempFile "openvpn.conf" $ \confPath confHandle -> do
    Text.hPutStr confHandle conf
    hSeek confHandle AbsoluteSeek 0
    processConfig <- openVpn confPath
    bracket (startProcess processConfig) (interruptProcessGroupOf . unsafeProcessHandle) $ \process -> do
        let output = getStdout process
        hSetBuffering output LineBuffering  -- By now, the connection is established.
        loopM analyzeOutputLine output
        ioResult <- io
          -- If there is anything else in the handle, it means connection was shaky and some events
          -- were logged.
        isShaky <- hReady output `catchIOError` \e -> if isEOFError e then return True else ioError e
        result <- if isShaky
                        then return $ Left . Error $ "VPN failure."
                        else return $ Right ioResult
        return result
  where
    openVpn confPath = do
        let confPathUntyped = fromAbsFile confPath
        user  <- fmap (filter isAlphaNum . toString) $ readProcessStdout_ "id --user  --name"
        group <- fmap (filter isAlphaNum . toString) $ readProcessStdout_ "id --group --name"
        return $ setStdout createPipe
            $ proc "sudo" ["openvpn", "--user", user, "--group", group, "--config", confPathUntyped]

    analyzeOutputLine handle = do
        line <- Text.hGetLine handle
        return case parseMaybe connected line of
            Nothing -> Left handle
            Just _  -> Right ()

    connected :: Parsec Void Text String
    connected = manyTill anySingle (string "Initialization Sequence Completed")
