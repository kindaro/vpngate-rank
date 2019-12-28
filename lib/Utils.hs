{-# language TypeFamilyDependencies, MultiParamTypeClasses #-}

module Utils where

import RIO
import RIO.Orphans ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Process.Typed
import Data.ByteString.UTF8 (toString)
import RIO.ByteString.Lazy (toStrict)
import Data.Witherable
import Text.Printf
import RIO.Text (pack)

import Types

import Control.Sequencer

class Diag (c :: * -> * -> *) a where
    type Source c a

    diag :: Source c a -> c a a

instance Diag (,) a where
    type Source (,) a = a

    diag x = (x, x)

instance Ord a => Diag Map a where
    type Source Map a = [a]

    diag = Map.fromList . fmap diag

independent_ :: (Witherable w, HasLogFunc env) => w (RIO env a) -> RIO env (w a)
independent_ = independent [handleAllSynchronous] log
  where
    log :: SomeException -> RIO _ ()
    log x = logWarn ("Independent branch exception: " <> display x)

insistent_ :: HasLogFunc env => Int -> RIO env a -> RIO env a
insistent_ = insistent [handleAllSynchronous] log
  where
    log :: SomeException -> RIO _ ()
    log x = logWarn ("Redundant branch exception: " <> display x)

-- | Run a process. If successful, return StdOut. If non-zero exit code, throw StdErr.
getProc :: HasLogFunc env => ByteString -> [ByteString] -> RIO env ByteString
getProc prog args = do
    logInfo $ "Running external program: "
            <> displayShow prog
            <> (mconcat . fmap ((" " <>) . displayShow)) args
    (exitCode, stdOut, stdErr) <- readProcess (proc' prog args)
    case exitCode of
        ExitSuccess   -> return (toStrict stdOut)
        ExitFailure _ -> throwM ((ProcessException . toString . toStrict) stdErr)

  where proc' prog' args' = proc (toString prog') (fmap toString args')

-- Grace a Chris Taylor https://stackoverflow.com/a/19724090
getMaxFromMap :: Ord v => Map k v -> [k]
getMaxFromMap m = go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

-- | Display bits per second value as megabytes per second, with up to 2 decimal digits of
-- precision.
showAsMbps :: Double -> Text
showAsMbps = pack . printf "%.2fMBps" . (/ 2^23)
