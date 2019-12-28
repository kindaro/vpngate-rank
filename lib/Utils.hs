{-# language TypeFamilyDependencies, MultiParamTypeClasses #-}

module Utils where

import RIO
import RIO.Orphans ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Process.Typed
import Data.ByteString.UTF8 (toString)
import RIO.ByteString.Lazy (toStrict)

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

independent_ :: HasLogFunc env => [RIO env a] -> RIO env [a]
independent_ = independent [handleAllSynchronous] (logWarn . displayShow @SomeException)

insistent_ :: HasLogFunc env => Int -> RIO env a -> RIO env a
insistent_ = insistent [] (logWarn . displayShow @SomeException)

-- | Run a process. If successful, return StdOut. If non-zero exit code, throw StdErr.
getProc :: ByteString -> [ByteString] -> RIO env ByteString
getProc prog args = do
    (exitCode, stdOut, stdErr) <- readProcess (proc' prog args)
    case exitCode of
        ExitSuccess -> return (toStrict stdOut)
        ExitFailure _ -> fail (show stdErr)

  where proc' prog' args' = proc (toString prog') (fmap toString args')
