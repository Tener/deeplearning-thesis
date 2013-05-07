module Main where

import BreakthroughGame
import Board

import GenericGameExperiments
import GenericGame

main = print =<< sampleGamesTrainNetwork (freshGameDefaultParams :: Breakthrough) 1000000 10.01 Nothing
