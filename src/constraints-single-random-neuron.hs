module Main where

import Constraints
import Control.Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do
  threads <- getNumCapabilities
  let target = 1
  asyncs <- mapM (\ threadNum -> async (singleNeuronRandomSearch target threadNum)) [1..threads]
  (_, solution) <- waitAny asyncs

  print "Async returned... exiting"
  print solution
  
  return ()

  
