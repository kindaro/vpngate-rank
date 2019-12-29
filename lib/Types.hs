module Types where

import RIO
import qualified RIO.Process
import qualified Data.Text.IO as Text
import Data.Char


-- * Convenient type synonyms.

type Url = ByteString


-- * Various pieces of data.

data Meta


-- * Exceptions.

data EncodingException = EncodingException String deriving Show

instance Exception EncodingException where
    displayException (EncodingException s) = s

data ProcessException = ProcessException String deriving Show

instance Exception ProcessException where
    displayException (ProcessException s) = if null (filter (not . isSpace) s)
        then "Process exited with error."
        else "Process exited with error. StdErr: \n" ++ s


-- * App type and classes.

class HasTmpDir env

class HasHumanInteraction env where
    humanMessageL :: Lens' env (Text -> IO ())

data App = App
    { appLogFunc :: !LogFunc
    , appProcessContext :: !RIO.Process.ProcessContext
    , appHumanMessage :: Text -> IO ()
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance RIO.Process.HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasHumanInteraction App where
    humanMessageL = lens appHumanMessage _u

runApp :: RIO App a -> IO a
runApp inner = runSimpleApp $ do
  logFunc <- view logFuncL
  processContext <- view RIO.Process.processContextL
  let app = App
        { appLogFunc = logFunc
        , appProcessContext = processContext
        , appHumanMessage = Text.putStrLn
        }
  runRIO app inner

message :: HasHumanInteraction env => Text -> RIO env ()
message s = do
    message <- view humanMessageL
    liftIO $ message s
