module Main where

import GenericGame
import AgentGeneric
import BreakthroughGame
import Board
import HistogramGeneric
import ThreadLocal

main = runThrLocMainIO $ do
  histogramRandomGameDepth (freshGameDefaultParams :: Abalone) "histogram-abalone.svg" 1000
  histogramRandomGameDepth (freshGameDefaultParams :: Breakthrough) "histogram-breakthrough.svg" 1000
