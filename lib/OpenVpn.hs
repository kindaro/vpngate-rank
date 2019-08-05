module OpenVpn where

import System.Process.Typed
import Path
import Text.Megaparsec
import Text.Megaparsec.Byte
import Control.Monad.Combinators
import Data.Void
import Control.Monad.Extra
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO
import Data.Either
import System.IO.Error
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Lazy.UTF8
import Data.Char
import System.Process (interruptProcessGroupOf)
import Control.Exception

import Types
import Iperf
import JsonIperf

newtype Conf = Conf { conf :: Path Abs File } deriving (Show, Eq, Ord)

withOpenVpn :: Conf -> IO a -> IO (Either Error a)
withOpenVpn Conf{..} io = do
    processConfig <- openVpn
    bracket (startProcess processConfig) (interruptProcessGroupOf . unsafeProcessHandle) $ \process -> do
        let output = getStdout process
        hSetBuffering output LineBuffering  -- By now, the connection is established.
        loopM analyzeOutputLine output
        putStrLn "Connected."
        ioResult <- io
        putStrLn "Measurements obtained."
          -- If there is anything else in the handle, it means connection was shaky and some events
          -- were logged.
        isShaky <- hReady output `catchIOError` \e -> if isEOFError e then return True else ioError e
        result <- if isShaky
                        then do
                            log <- hGetContents output
                            putStrLn log
                            return $ Left . Error $ "VPN failure: " ++ conf'
                        else return $ Right ioResult
        if isRight result
           then putStrLn "Right result achieved."
           else putStrLn $ "Error has occured."
        return result
  where
    openVpn = do
        user  <- fmap (filter isAlphaNum . toString) $ readProcessStdout_ "id --user  --name"
        group <- fmap (filter isAlphaNum . toString) $ readProcessStdout_ "id --group --name"
        return $ setStdout createPipe
            $ proc "sudo" ["openvpn", "--user", user, "--group", group, "--config", conf']

    conf' = fromAbsFile conf

    analyzeOutputLine handle = do
        line <- Text.hGetLine handle
        Text.putStrLn $ "openvpn: " <> line
        return case parseMaybe connected line of
            Nothing -> Left handle
            Just _  -> Right ()

    connected :: Parsec Void Text String
    connected = manyTill anySingle (string "Initialization Sequence Completed")
