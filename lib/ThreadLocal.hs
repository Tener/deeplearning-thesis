{-# LANGUAGE ImplicitParams, Rank2Types, BangPatterns #-}

module ThreadLocal where

import Control.Concurrent
import Text.Printf
import System.IO

type ThrLocIO a = (?thrLoc :: ThreadLocal) => IO a
data ThreadLocal = ThreadLocal { tl_stdout :: MVar Handle
                               , tl_ident :: String
                               }

runThrLocMainIO :: (ThrLocIO a) -> IO a
runThrLocMainIO main = do
  var <- newMVar stdout
  runThrLocIO (ThreadLocal var "MAIN") main

runThrLocIO :: ThreadLocal -> ThrLocIO a -> IO a
runThrLocIO tl ma = let ?thrLoc = tl in ma

putStrLnTL :: String -> ThrLocIO ()
putStrLnTL val = do
  let msg = (printf "[THR=%s] %s" (tl_ident ?thrLoc) (val :: String))
  msg `seq` modifyMVar_ (tl_stdout ?thrLoc) (\ handle -> do
                                               hPutStrLn handle msg
                                               return handle)

printTL :: (Show a) => a -> ThrLocIO ()
printTL val = putStrLnTL (show val)
