module Main where

import BreakthroughGame
import Board

import GenericGameExperiments
import GenericGame
import ThreadLocal

main = runThrLocMainIO $ do
         print =<< sampleGamesTrainNetwork (freshGameDefaultParams :: Breakthrough) 1000000 10.01 Nothing
