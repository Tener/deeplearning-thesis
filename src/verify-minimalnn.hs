module Main where

import MinimalNN
import BreakthroughGame
import GenericGame
import AgentGeneric
import ThreadLocal

import System.Random.MWC
import System.Environment

import qualified Data.Vector as V
import qualified MyVectorType as MV

main = runThrLocMainIO $ do
  [fin] <- getArgs
  net <- MinimalNN.decodeFile fin
  agSmpl <- mkAgent net
  fixedgen <- initialize (V.fromList [1..256])
  let agRnd = AgentRandom fixedgen
      pp = putStrLn . prettyPrintGame
  print =<< driverG2 (freshGameDefaultParams :: Breakthrough) (agSmpl :: AgentSimple) agRnd (GameDriverCallback pp (\ g p -> print p >> pp g >> return True))

  return ()
  
