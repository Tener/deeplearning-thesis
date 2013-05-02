module Main where

import BreakthroughGame
import GenericGameExperiments
import GenericGame

main = print =<< sampleGamesTrainNetwork (freshGameDefaultParams :: Breakthrough) 100000 0.01
