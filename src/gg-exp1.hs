module Main where

import BreakthroughGame
import GenericGameExperiments
import GenericGame

main = sampleGamesTrainNetwork (freshGameDefaultParams :: Breakthrough) 10000
