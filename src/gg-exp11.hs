{-# LANGUAGE ViewPatterns, TemplateHaskell, TypeFamilies, UndecidableInstances, Rank2Types, FlexibleContexts #-}
-- tournament between neural networks - each working with the same DBN.

module Main where

import Prelude hiding (putStr, putStrLn)

import GenericGameExperiments
import GenericGame
import AgentGeneric
import BreakthroughGame
import ConstraintsGA -- NN1 comes from here
import Matlab
import MinimalNN
import MinimalGA
import NeuralNets
import ThreadLocal
import Utils
import THUtils

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Maybe
import Data.List.Split (splitEvery)
import Data.List (transpose)
import Data.Default
import Data.IORef
import Data.Timeout
import System.Random.MWC
import Text.Printf

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type DBN = TNetwork

-- | this is our working element. first is DBN, then single layer of interpeters, finally summed by (virtual) neuron at the end with weights tied to every neuron.
--   the @g@ type param fixes game type, @ag@ type param fixes agent type.
data EvolNet ag g = EvolNet DBN [(NN1, Double)] deriving (Show,Eq,Ord,Read)
type MyEntType = EvolNet (AgentSimple TNetwork) Breakthrough

dbnGameCount = 100000
dbnGameProb = 0.1
dbnMatlabOpts = Just (def {dbnSizes = [25], numEpochs = 5, implementation = Matlab})
mctsLevel = 1000

main :: IO ()
main = runThrLocMainIO $ do
  printTL "exp11::start new run"
  printTL "source code for this experiment: "
  putStrLnTL $(quoteThisFile)
  printTL "DBN read/train"
  fn <- sampleGamesTrainNetworkSolveMCTS (freshGameDefaultParams :: MyGame) dbnGameCount dbnGameProb dbnMatlabOpts mctsLevel
  printTL ("DBN FN=",fn)
  (dbn,dbnLayerSizes) <- parseNetFromFile <$> readFile fn
  let dbnLastLayerSize = last dbnLayerSizes
  threads <- getNumCapabilities

  printTL "exp11::dbn start"
  printTL dbn
  printTL "exp11::dbn end"
  printTL =<< evaluateLL dbn "???"

  printTL "exp11::finished"
