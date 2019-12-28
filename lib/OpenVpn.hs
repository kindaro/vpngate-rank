module OpenVpn where

import RIO

import System.Process.Typed
import Path
import Text.Megaparsec
import Text.Megaparsec.Byte
import Control.Monad.Extra
import RIO.Text (Text)
import qualified Data.Text.IO as Text
import System.IO.Error
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Char
import System.Process (interruptProcessGroupOf)
import System.IO (SeekMode(..))

import Types

default (Text)

newtype Conf = Conf { conf :: Text } deriving (Show, Eq, Ord)

-- withSystemTempFileTyped :: (MonadIO m, MonadMask m)
--                         => Strict.ByteString -> (Path Abs File -> Handle -> m a) -> m a
-- withSystemTempFileTyped pattern action = withSystemTempFile (StrictUTF8.toString pattern) $ \name' handle -> do
--     name <- parseAbsFile name'
--     action name handle

-- withOpenVpn :: forall m env a. (MonadIO m, MonadReader env m, HasLogFunc env, MonadUnliftIO m, MonadThrow m)
--             => Conf -> m a -> m (Either Error a)
-- withOpenVpn Conf{..} io = do
--     withSystemTempFile "openvpn.conf" $ \confPath' confHandle -> do
--         confPath <- parseAbsFile confPath'
--         hClose confHandle
--         doStuff confPath
-- 
--   where
--     doStuff :: Path Abs File -> m (Either Error a)
--     doStuff confPath = do
--         writeFileUtf8 (fromAbsFile confPath) conf
--         processConfig <- openVpn confPath
--         bracket (liftIO $ startProcess processConfig) (liftIO . interruptProcessGroupOf . unsafeProcessHandle) $ \process -> do
--             let output = getStdout process
--             hSetBuffering output LineBuffering  -- By now, the connection is established.
--             liftIO $ loopM analyzeOutputLine output
--             ioResult <- io
--               -- If there is anything else in the handle, it means connection was shaky and some events
--               -- were logged.
--             isShaky <- liftIO $ hReady output `catchIOError` \e -> if isEOFError e then return True else ioError e
--             result <- if isShaky
--                             then return $ Left . Error $ "VPN failure."
--                             else return $ Right ioResult
--             return result
-- 
--     openVpn confPath = do
--         let confPathUntyped = fromAbsFile confPath
--         user  <- fmap (filter isAlphaNum . toString) $ readProcessStdout_ "id --user  --name"
--         group <- fmap (filter isAlphaNum . toString) $ readProcessStdout_ "id --group --name"
--         return $ setStdout createPipe
--             $ proc "sudo" ["openvpn", "--user", user, "--group", group, "--config", confPathUntyped]
-- 
--     analyzeOutputLine h = do
--         line <- Text.hGetLine h
--         Text.putStrLn line
--         return case parseMaybe connected line of
--             Nothing -> Left h
--             Just _  -> Right ()
-- 
--     connected :: Parsec Void Text String
--     connected = manyTill anySingle (string "Initialization Sequence Completed")
