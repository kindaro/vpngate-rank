module Helpers where

import Prelude
import Control.Exception (SomeAsyncException, ArithException(Overflow), throw)
import Control.Monad.Catch
import System.Random

-- I will maybe call this module "sequence" or something like that.

-- Run all of the action and collect all the results and synchronous exceptions.
--
-- An asynchronous exception must lead to immediate termination.
--
independent :: (Monad (m e), Foldable f, Monoid (q e), Monoid (q a))
            => f (m e a) -> m (q e) (q a)

independent actions = undefined

insistent :: MonadCatch m => Word -> m a -> m ([SomeException], Maybe a)
insistent 0 action = return ([ ], Nothing)
insistent n action = do
    x <- handleSynchronous action
    case x of
        Left e -> do
            (es, ma) <- insistent (n - 1) action
            return (e: es, ma)
        Right y -> return ([ ], Just y)



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

data Exceptions = forall e. Exception e => Exceptions e

handleSynchronous :: MonadCatch m => m a -> m (Either SomeException a)
handleSynchronous action = fmap Right action `catches`
    [ Handler (throw :: SomeAsyncException -> w), Handler \e -> (return . Left) e ]

oftenFail = randomRIO (1 :: Word, 10) >>= \x -> if x /= 7 then throw Overflow else return ()
