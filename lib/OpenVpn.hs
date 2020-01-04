module OpenVpn where

import RIO hiding (withTempFile)
import RIO.Process
import Path
import Path.IO
import Text.Megaparsec
import Text.Megaparsec.Byte
import Control.Monad.Extra
import RIO.Text (Text)
import qualified Data.Text.IO as Text
import System.IO.Error
import Data.Char
import System.Process (terminateProcess)
import System.IO (SeekMode(..))
import Data.String.Conv
import RIO.List (unfoldr)

import Types
import App

-- How is this going to work?
-- There are 3 layers of brackets.
--     1. Temporary file create / destroy.
--          - I will handle this with RIO.
--     2. Process launch / terminate.
--     3. Parse connection estblished / errors.
--
-- Within these brackets, I can do anything.
-- But there are complications.
--
-- 1. The `openvpn` process must be monitored in a parallel thread.
-- 2. It is difficult to kill properly.
-- 3. I must be careful when creating temporary configuration files, so as not to overwrite
-- anything.
--
-- There are 2 processes: `openvpn` and `action`. They race. Once one fails, I must interrupt the
-- other. How can I accomplish this? With asynchronous exceptions, I suppose. But then I need two
-- threads that are both disposable, right?
--
-- Given the callback hell, I should probably reach for continuation.
--
-- So, I race the `openvpn` thread and the `action` thread. But `action` should wait until
-- `openvpn` is established. Also, `action` should be terminated as soon as possible, without
-- waiting for `openvpn` to shut down. `race` is not a very suitable primitive. Rather, I should
-- use a variable.


withOpenVpn :: (HasProcessContext env, HasLogFunc env, HasTmpDir env)
            => Text -> RIO env a -> RIO env a
withOpenVpn conf action = do
    tmpDir <- view tmpDirL
    withTempFile tmpDir "openvpn.ovpn" \confPath h -> do
        hClose h
        writeFileUtf8 (fromAbsFile confPath) conf
        openVpn confPath action

--   where
--     doStuff :: Path Abs File -> m (Either Error a)
--     doStuff confPath = do
--         writeFileUtf8 (fromAbsFile confPath) conf
--         processConfig <- openVpn confPath
--         bracket (liftIO $ startProcess processConfig) (liftIO . interruptProcessGroupOf . unsafeProcessHandle) $ \process -> do
--             let output = getStdout process
--             hSetBuffering output LineBuffering  -- By now, the connection is established.
--             liftIO $ loopM waitForConnection output
--             ioResult <- io
--               -- If there is anything else in the handle, it means connection was shaky and some events
--               -- were logged.
--             isShaky <- liftIO $ hReady output `catchIOError` \e -> if isEOFError e then return True else ioError e
--             result <- if isShaky
--                             then return $ Left . Error $ "VPN failure."
--                             else return $ Right ioResult
--             return result

openVpn :: forall env a. (HasProcessContext env, HasLogFunc env) => Path Abs File -> RIO env a -> RIO env a
openVpn confPath action = runOpenVpn confPath
    \process -> bracket_ (initialize process) (terminate process) (runAction process)
  where
    runAction :: Process () Handle () -> RIO env a
    runAction process = do
        r <- race (void (hWaitForInput (getStdout process) (-1))) action
        case r of
            Left () -> throwM (OpenVpnException "OpenVpn terminated early.")
            Right x -> return x

    initialize :: Process () Handle () -> RIO env ()
    initialize process = do
        let output = getStdout process
        hSetBuffering output LineBuffering  -- By now, the connection is established.
        waitForConnection output

    terminate :: Process () Handle () -> RIO env ()
    terminate process = do
        liftIO $ terminateProcess (unsafeProcessHandle process)
        log <- liftIO (Text.hGetContents (getStdout process))
        logInfo $ display log

-- | The specifics of running an `openvpn` process.
runOpenVpn
    :: (HasProcessContext env, HasLogFunc env)
    => Path Abs File -> (Process () Handle () -> RIO env a) -> RIO env a
runOpenVpn confPath action' = do
    user  <- fmap (init . toS) $ readProcessStdout_ "id --user  --name"
    group <- fmap (init . toS) $ readProcessStdout_ "id --group --name"
    let options = ["--user", user, "--group", group, "--config", fromAbsFile confPath]
    proc "openvpn" options \processConfig' -> do
        let processConfig = setStdout createPipe processConfig'
        withProcessWait_ processConfig action'

-- stopOpenVpn :: (HasProcessContext env, HasLogFunc env) => Process () Handle () -> RIO env ()
-- stopOpenVpn process = stopProcess process


sleeper :: MVar () -> RIO env a -> RIO env a
sleeper trigger dream = do
    alarm  -- Time to sleep.
    x <- race alarm dream  -- Either see the dream or wake up early.
    case x of
        Left () -> fail ""
        Right r -> return r
  where
    alarm = takeMVar trigger

init :: [a] -> [a]
init = reverse . drop 1 . reverse

waitForConnection :: HasLogFunc env => Handle -> RIO env ()
waitForConnection = whileM . fmap (isNothing . parseMaybe connected) . hGetLineWithLog
  where
    connected :: Parsec Void Text ()
    connected = void (manyTill anySingle (string "Initialization Sequence Completed"))

hGetLineWithLog :: HasLogFunc env => Handle -> RIO env Text
hGetLineWithLog h = do
    x <- liftIO $ Text.hGetLine h
    logInfo $ display x
    return x
