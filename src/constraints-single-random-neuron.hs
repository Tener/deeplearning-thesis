module Main where

import Constraints
import Control.Concurrent
import Control.Concurrent.Async
import Text.Printf
import Data.IORef

main :: IO ()
main = do
  threads <- getNumCapabilities
  best'chan <- newChan
  best'ref <- newIORef (undefined, -1)
  let target = 0.95
      getBest old@(best'ne,best'sc, _) = do
         new@(nbn, nbs, action) <- readChan best'chan
         if (nbs > best'sc)
          then 
             do
               action
               putStrLn (printf "[T] NEURON %s" (show nbn))
               putStrLn (printf "[T] SCORE %f " nbs)
               writeIORef best'ref (nbn,nbs)
               getBest new
          else getBest old

      runBestThrAndLocalSearch = do
         new@(nbn, nbs, action) <- readChan best'chan
         writeIORef best'ref (nbn, nbs)
         forkIO (getBest new)
  
  let threadsGlobal = max 1 (threads - threadsLocal)
      threadsLocal = max 1 (threads-1)

      newBest = (writeChan best'chan)
      localRange = 0.02

  asyncs'global <- mapM (\ threadNum -> async (singleNeuronRandomSearch newBest target threadNum)) [1..threadsGlobal]
  runBestThrAndLocalSearch
  asyncs'local <- mapM (\ threadNum -> async (singleNeuronLocalSearch newBest best'ref localRange target threadNum)) [1..threadsLocal]

  
  async'timeout <- async (do
                           let sec = 10^6
                           threadDelay (1 * 60 * sec)
                           best'baseline <- readIORef best'ref
                           let go old'best = do
                                 threadDelay (1 * 60 * sec) -- quit if we dont improve in 1 minute
                                 new'best <- readIORef best'ref
                                 if new'best == old'best then return old'best else go new'best
                                 
                           go best'baseline
                         )

  let asyncs = asyncs'local ++ asyncs'global ++ [async'timeout]

  (_, solution) <- waitAny asyncs

  print "Async returned... exiting"
  print solution
  
  return ()

  
