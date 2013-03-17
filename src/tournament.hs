-- the 'tournament' executable: run evolutionary algorithm on population of agents and breed a champion!

module Main where

import Tournament
import NeuralNets
import Data.Default

main :: IO ()
main = do
  pop <- (evolution Nothing def :: IO (Population AgentNN))
  prettyReportPopulation pop

