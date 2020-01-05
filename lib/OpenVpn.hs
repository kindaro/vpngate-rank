module OpenVpn where

import           RIO                  hiding (withTempFile)
import           RIO.Process

import qualified RIO.Text             as Text
import           Control.Monad.Loops  (unfoldM)
import qualified Data.Text.IO         as Text
import           Path
import           Path.IO
import           System.Process       (terminateProcess)
import           Text.Megaparsec      (Parsec, anySingle, manyTill, parseMaybe)
import           Text.Megaparsec.Byte (string)

import           App
import           Types

-- | Launch `openvpn` process with the given configuration, wait for it to connect, run the given
-- action and disconnect.
withOpenVpnConf :: (HasProcessContext env, HasLogFunc env, HasTmpDir env)
            => Text -> RIO env a -> RIO env a
withOpenVpnConf conf action = do
    tmpDir <- view tmpDirL
    withTempFile tmpDir "openvpn.ovpn" \confPath h -> do
        hClose h
        writeFileUtf8 (fromAbsFile confPath) conf
        withOpenVpn confPath action

-- | Launch `openvpn` process with the given configuration file, wait for it to connect, run the
-- given action and disconnect.
withOpenVpn
    :: forall env a. (HasProcessContext env, HasLogFunc env)
    => Path Abs File -> RIO env a -> RIO env a
withOpenVpn confPath action = runOpenVpn confPath
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
        hSetBuffering output LineBuffering
        itSays <- waitForConnection output
        logDebug $ display itSays

    terminate :: Process () Handle () -> RIO env ()
    terminate process = do
        liftIO $ terminateProcess (unsafeProcessHandle process)
        itSays <- liftIO $ Text.hGetContents (getStdout process)
        logDebug $ display itSays

-- | The specifics of running an `openvpn` process.
runOpenVpn
    :: (HasProcessContext env, HasLogFunc env)
    => Path Abs File -> (Process () Handle () -> RIO env a) -> RIO env a
runOpenVpn confPath action' = do
    -- user  <- getRealUser
    -- group <- getRealGroup
    let options =
          [ "--config", fromAbsFile confPath
          -- , "--user", toS user
          -- , "--group", toS group
          , "--connect-retry-max", "1"  -- Try to connect once, then exit.
          , "--resolv-retry", "0"  -- If name resolution fails, exit.
          , "--connect-timeout", "10"  -- Wait for 10 seconds for the remote to answer.
          ]
    proc "openvpn" options \processConfig' -> do
        let processConfig = setStdout createPipe processConfig'
        withProcessWait_ processConfig action'

waitForConnection :: Handle -> RIO env Text
waitForConnection h = fmap Text.unlines $ unfoldM do
    line <- liftIO $ Text.hGetLine h
    return case parseMaybe connected line of
        Just () -> Nothing
        Nothing -> Just line

  -- whileM . fmap (isNothing . parseMaybe connected) . hGetLineWithLog
  where
    connected :: Parsec Void Text ()
    connected = void (manyTill anySingle (string "Initialization Sequence Completed"))
