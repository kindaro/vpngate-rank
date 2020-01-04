module App where

import RIO
import RIO.Orphans
import qualified RIO.Process
import qualified Data.Text.IO as Text
import Path
import Path.IO
import Constants
import Types

class HasTmpDir env where
    tmpDirL :: Lens' env (Path Abs Dir)

class HasHumanInteraction env where
    humanMessageL :: Lens' env (Text -> IO ())

data App = App
    { appLogFunc :: !LogFunc
    , appProcessContext :: !RIO.Process.ProcessContext
    , appHumanMessage :: Text -> IO ()
    , appTmpDir :: Path Abs Dir
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance RIO.Process.HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasHumanInteraction App where
    humanMessageL = lens appHumanMessage (\x y -> x { appHumanMessage = y })

instance HasTmpDir App where
    tmpDirL = lens appTmpDir (\x y -> x { appTmpDir = y })

runApp :: RIO App a -> IO a
runApp inner = runSimpleApp $ do
  logFunc <- view logFuncL
  processContext <- view RIO.Process.processContextL
  withSystemTempDir programName \tmpDir -> do
      let app = App
            { appLogFunc = logFunc
            , appProcessContext = processContext
            , appHumanMessage = Text.putStrLn
            , appTmpDir = tmpDir
            }
      runRIO app inner

message :: HasHumanInteraction env => Text -> RIO env ()
message s = do
    message <- view humanMessageL
    liftIO $ message s
