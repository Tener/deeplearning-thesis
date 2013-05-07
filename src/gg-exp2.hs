module Main where

import BreakthroughGame
import Board


import GenericGameExperiments
import GenericGame
import AgentGeneric

type MyGame = Breakthrough

someGame :: MyGame
someGame = freshGameDefaultParams

main = do
  fn <- sampleGamesTrainNetwork (freshGameDefaultParams :: MyGame) 100000 1 Nothing
  agSmp <- mkAgentSimpleFile fn
  agRnd <- mkAgent () :: IO AgentRandom
  agMT <- mkAgent 5 :: IO AgentMCTS
  sampleGameDepth agSmp agMT
                  (return True)
                  (\ g depth -> do
                     print ("winner", winner (g :: MyGame), "depth", depth)
                 --     putStrLn $ prettyPrintGame g
                  )
                  
  
