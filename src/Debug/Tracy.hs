module Debug.Tracy
  ( makeTracy
  , tracy
  , tracyM
  , ftracy
  , ftracyM
  , ftracyAp
  ) where

import Control.Monad
import Debug.Trace
import System.Environment
import System.IO.Unsafe

-- | Fetch an environment variable using 'unsafePerformIO'.
-- This is done only once per program run because CAFs are
-- evaluated only once.
{-# NOINLINE unsafeLookupEnv #-}
unsafeLookupEnv :: String -> Maybe String
unsafeLookupEnv envVar = unsafePerformIO (lookupEnv envVar)

-- | Trace only if an environment variable is set and its
-- value satisfies a predicate.
makeTracy :: String -> (String -> Bool) -> String -> (a -> a)
makeTracy envVar p =
  if maybe False p (unsafeLookupEnv envVar)
    then trace
    else \_msg -> id

-- | Trace only if the @DEBUG@ environment variable is set to @TRUE@.
tracy :: String -> a -> a
tracy = makeTracy "DEBUG" (=="TRUE")

tracyM :: (Monad m) => String -> m ()
tracyM x = tracy x $ return ()

ftracy :: Show a => (String -> String) -> a -> a
ftracy f x = tracy (f $ show x) x

ftracyAp :: (Show a, Monad m) => (String -> String) -> a -> m a
ftracyAp f x = ftracyM f x >> return x

ftracyM :: (Show a, Monad m) => (String -> String) -> a -> m ()
ftracyM f x = tracyM (f $ show x)
