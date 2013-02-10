module Main where

import Constraints
import Control.Concurrent
import Control.Concurrent.Async
import Text.Printf

main :: IO ()
main = do
  threads <- getNumCapabilities
  best'chan <- newChan
  let target = 1
      getBest old@(best'ne,best'sc, _) = do
         new@(nbn, nbs, action) <- readChan best'chan
         if (nbs > best'sc)
          then 
             do
               action
               putStrLn (printf "[T] NEURON %s" (show nbn))
               putStrLn (printf "[T] SCORE %f " nbs)
               getBest new
          else getBest old
             
  forkIO $ getBest (undefined,negate (1/0), return ())
    
  asyncs <- mapM (\ threadNum -> async (singleNeuronRandomSearch (writeChan best'chan) target threadNum)) [1..threads]
  (_, solution) <- waitAny asyncs

  print "Async returned... exiting"
  print solution
  
  return ()

  
