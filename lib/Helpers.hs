module Helpers where

import Prelude
import Control.Exception (SomeAsyncException, ArithException(Overflow), throw)
import Control.Monad.Catch
import Control.Monad.Writer.Strict
import System.Random
import Control.Applicative

-- I will maybe call this module "sequence" or something like that.

-- Run all of the action and collect all the results and synchronous exceptions.
--
-- An asynchronous exception must lead to immediate termination.
--
independent :: (Monad (m e), Foldable f, Monoid (q e), Monoid (q a))
            => f (m e a) -> m (q e) (q a)

independent actions = undefined

insistent :: (MonadCatch m, MonadWriter (q SomeException) m, Alternative q)
          => Word -> m a -> m a
insistent 0 action = throwM SequencingFailure
insistent n action = action `catchSynchronous` \e -> do
                                                        tell (pure e)
                                                        insistent (n - 1) action



redundant :: (Monad (m e), Foldable f, Monoid (q e))
          => f (m e a) -> m (q e) a
redundant = undefined

-- Log everything going in and out, and provide the same for analysis.
--
-- Logging is a monadic action, so loggers can be sewn inside the environment.
--
-- We must edit the process description to launch the `timeout` program available on Unix.
--
-- transparent :: ProcessDescription a b c -> 

data SequencingFailure = SequencingFailure deriving Show
instance Exception SequencingFailure where displayException _ = "sequencing failure"

catchSynchronous :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catchSynchronous action handler = action `catches`
    [ Handler (throw :: SomeAsyncException -> w), Handler handler ]

oftenFail = randomRIO (1 :: Word, 10) >>= \x -> if x /= 7 then throw Overflow else return ()
