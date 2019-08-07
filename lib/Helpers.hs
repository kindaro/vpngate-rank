module Helpers where

import Prelude
import Control.Exception


-- I will maybe call this module "sequence" or something like that.

-- Run all of the action and collect all the results and synchronous exceptions.
--
-- An asynchronous exception must lead to immediate termination.
--
independent :: (Monad (m e), Foldable f, Monoid (q e), Monoid (q a))
            => f (m e a) -> m (q e) (q a)

independent actions = undefined

insistent :: (Monad (m e), Monoid (q e))
          => Word -> m e a -> m (q e) (Maybe a)
insistent = undefined

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

handleSynchronous :: IO a -> IO (Either SomeException a)
handleSynchronous action = fmap Right action `catches`
    [ Handler (throw :: SomeAsyncException -> w), Handler \e -> (return . Left) e ]
