module Debug.Tracy
  ( makeTracy
  , tracy
  , tracyM
  , ftracy
  , ftracyM
  ) where

import Control.Monad
import Debug.Trace
import System.Environment
import System.IO.Unsafe

-- | Wraps an unsafe tracing/logging action.
{-# NOINLINE optional #-}
optional :: String -> String -> (a -> a) -> (a -> a)
optional envVal val action = unsafePerformIO $ do
  enabled <- lookupEnv envVal
  return $ case enabled of
    Just val -> action
    Nothing  -> id

makeTracy :: String -> String -> String -> (a -> a)
makeTracy envVal val = optional envVal val . trace

tracy :: String -> a -> a
tracy = makeTracy "DEBUG" "TRUE"

tracyM :: (Monad m) => String -> m ()
tracyM x = tracy x $ return ()

ftracy :: Show a => (String -> String) -> a -> a
ftracy f x = tracy (f $ show x) x

ftracyAp :: (Show a, Monad m) => (String -> String) -> a -> m a
ftracyAp f x = ftracyM f x >> return x

ftracyM :: (Show a, Monad m) => (String -> String) -> a -> m ()
ftracyM f x = tracyM (f $ show x)
