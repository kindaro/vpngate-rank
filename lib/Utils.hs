{-# language TypeFamilyDependencies, MultiParamTypeClasses #-}

module Utils
    ( getProc
    , Diag
    , diag
    , independent_
    , insistent_
    , cool_
    , showAsMbps
    , getMaxFromMap
    , checkRootOrExit
    , getRealUser
    , getRealGroup
    , fibers
    , unfibers
    )
    where

import           RIO
import           RIO.Orphans         ()
import           RIO.Process
import qualified RIO.Text            as Text

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.String.Conv
import           Data.Witherable     (Witherable)
import qualified RIO.ByteString.Lazy as Lazy
import           Text.Printf

import           Control.Sequencer

-- import Types ()

class Diag (c :: * -> * -> *) a where
    type Source c a

    diag :: Source c a -> c a a

instance Diag (,) a where
    type Source (,) a = a

    diag x = (x, x)

instance Ord a => Diag Map a where
    type Source Map a = [a]

    diag = Map.fromList . fmap diag

independent_ :: forall w env a. (Witherable w, HasLogFunc env) => w (RIO env a) -> RIO env (w a)
independent_ = independent [handleAllSynchronous] logException
  where
    logException :: SomeException -> RIO env ()
    logException x = logWarn ("Independent branch exception: " <> display x)

insistent_ :: forall env a. HasLogFunc env => Int -> RIO env a -> RIO env a
insistent_ = insistent [handleAllSynchronous] logException
  where
    logException :: SomeException -> RIO env ()
    logException x = logWarn ("Redundant branch exception: " <> display x)

cool_ :: forall env a. HasLogFunc env => Int -> Int -> RIO env a -> RIO env a
cool_ = cool [handleAllSynchronous] logException
  where
    logException :: SomeException -> RIO env ()
    logException x = logWarn ("Cool branch exception: " <> display x)

-- | Run a process, with lowered privileges if possible, return standard output.
getProc :: (HasProcessContext env, HasLogFunc env)
        => String -> [String] -> RIO env Lazy.ByteString
getProc prog args = do
    logInfo $ "Running external program: "
            <> displayShow prog
            <> (mconcat . fmap ((" " <>) . displayShow)) args
    checkRootOrExit
    env <- view envVarsL
    let userName = case Map.lookup "SUDO_USER" env <|> Map.lookup "USER" env of
            Nothing -> error "This should not happen: there is always USER environment variable."
            Just x  -> x
    args' <- if userName == "root"
                   then logWarn "Cannot drop privileges." >> return args
                   else return ("-u": toS userName: prog: fmap toS args)
    (stdOut, _) <- proc "sudo" args' \pc -> readProcess_ pc
    return stdOut

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
showAsMbps = Text.pack . printf "%.2fMBps" . (/ 2^(23 :: Int))

checkRootOrExit :: (HasProcessContext env, HasLogFunc env) => RIO env ()
checkRootOrExit = do
    env <- view envVarsL
    case Map.lookup "USER" env of
        Just "root" -> return ()
        Just _ -> do
            logError "Must be root."
            exitFailure
        Nothing -> error "This should not happen: there is always USER environment variable."

getRealUser :: HasProcessContext env => RIO env Text
getRealUser = do
    env <- view envVarsL
    case (listToMaybe . catMaybes) [Map.lookup "SUDO_USER" env, Map.lookup "USER" env] of
        Just x  -> return x
        Nothing -> error "This should not happen: there is always USER environment variable."

getRealGroup :: (HasProcessContext env, HasLogFunc env) => RIO env Text
getRealGroup = do
    env <- view envVarsL
    case Map.lookup "SUDO_GID" env of
        Just x  -> do
            entry <- proc "getent" ["group", toS x]
                \processConfig -> readProcessStdout_ processConfig
            let (group, _) = Text.break (== ':') (toS entry)
            return group
        Nothing -> do
            raw <- readProcessStdout_ "id --group --name"
            let (clean, _) = (Text.break (== '\n') . toS) raw
            return clean

fibers :: forall k v. Ord v => Map k v -> Map v [k]
fibers = Map.fromList . fmap fitting . classifyBy ((==) `on` snd) . Map.toList
  where
    fitting :: [(k, v)] -> (v, [k])
    fitting ((k, v): kvs) = (v, k: fmap fst kvs)
    fitting _ = error "This should not happen: `classifyBy` always returns non-empty classes."

unfibers :: [(v, [k])] -> [(k, v)]
unfibers = concatMap f where f (y, xs) = zip xs (pure y)

classifyBy :: forall a f. Foldable f => (a -> a -> Bool) -> f a -> [[a]]
classifyBy eq = foldl' f [ ]
  where
    f :: [[a]] -> a -> [[a]]
    f [ ] y = [[y]]
    f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
                          | otherwise = xs: f xss y
    f _ _ = error "This should not happen: empty lists are never created, only singletons."
