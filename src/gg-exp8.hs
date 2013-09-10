{-# LANGUAGE TemplateHaskell #-}
-- based on experiment 5, we try to find optimal set of constraints to use using evolutionary approach.

module Main where

import Prelude hiding (putStr, putStrLn)

import ConstraintsGA
import ConstraintsGeneric
import GenericGameExperiments
import Matlab
import MinimalNN
import NeuralNets
import ThreadLocal
import Utils
import THUtils

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Default
import Data.IORef
import Data.Timeout
import Text.Printf
import System.FilePath

import qualified Data.Map as Map
import Data.List (sortBy,sort)
import Data.Ord (comparing)

useCachedDBN = False
searchTimeout = 5 # Second
dbnGameCount = 100000
dbnGameProb = 0.07
dbnMatlabOpts = Just (def {dbnSizes = [1], numEpochs = 10, implementation = Matlab})
constraintSource = CS_Gameplay playerUseCoinstraints gameplayConstraints'0
playerUseCoinstraints = 100
localSearch = 0.003
evalCount = 12
mctsCount = 25
mctsIncrement = 1
bigStepDumpCount = 10
constraintSetSize = 20
veryGoodScore = 0.80

mkLayer :: [SingleNeuron] -> TNetwork
mkLayer neurons = let ws = [map ((\ [[w]] -> w ) . fst) neurons]
                      bs = [map ((\ [[b]] -> b ) . snd) neurons]
                  in mkTNetwork ws bs

getNeuronSize :: SingleNeuron -> Int
getNeuronSize ([[w]],_) = length w

main :: IO ()
main = runThrLocMainIO $ do
  printTL "source code for this experiment: "
  putStrLnTL showExperimentSource
  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  printTL ("DBN FN=",fn)
  dbn <- getDBNFile fn

  printTL "Constraint generation"
  constraints <- getConstraints constraintSource
  let constraintsPacked = map (packConstraint dbn) $ concatMap (uncurry generateConstraintsSimple) constraints
  printTL ("Total coinstraint count", length constraintsPacked)
  printTL "Evaluating packed constraints..."
  print $ head constraintsPacked
  printTL "Constraints:"
  printTL constraints
  threads <- getNumCapabilities
  gen <- mkGenIO

  -- constraintsPacked is our whole population to choose from.
  -- we have to find optimal set of constraints.
  -- the size of the set is determined by 'constraintSetSize' parameter.
  -- each constraint is assigned initial score of (1,0): 1 is initial weight, 0 is number of evaluations.
  -- at each 'big step' we pick a subset of constraints and evaluate their fitness: train fixed number (like 1) of interpreting subnetworks and run few test games.
  -- we track wins/loses and at the end of 'big step' we assign our scores: simply add percentage of won/lost games.
  -- in subsequent runs the better constraints are picked more often. we also track overall performance of 'best' constraint-set.
  --
  -- the experiment can basically go forever, but we can set some limit parameters to stop the progress once very good set of constraints has been found.
  -- it is important to use hyper-param values that make computation fast because the algorithm will be slow nonetheless.

  let initialScore = (10,0)
  constraintMap <- newIORef (Map.fromList (zip constraintsPacked (cycle [initialScore])))
  mctsCountVar <- newIORef mctsCount

  let pickSublistWeighted = do
        let bigOrDivide pct 0 = 10000
            bigOrDivide pct count = pct / fromIntegral count
        allMap <- (map (\ (const,(pct,count)) -> ((bigOrDivide pct count),const)) . Map.toList) <$> readIORef constraintMap
        shuffled <- shuffle allMap
        replicateM constraintSetSize (pickListWeighted gen shuffled)
      updateScores sublist newPct = do
        let updateFunc = (\ (oldPct,oldCnt) -> ((oldPct+newPct),(oldCnt+1)))
        mapM_ (\ key -> modifyIORef constraintMap (Map.adjust updateFunc key)) sublist
      trainInterpretingLayer subconstraints = do
        bestRef <- newIORef (undefined, neginf)
        let wt thr'act = waitAnyCancel =<< withTimeout bestRef searchTimeout (mapM thr'act [1..threads])
        (_, bestFinal) <- wt (\ thr -> async (singleNeuronMinimalGAReprSearch (searchCB bestRef) thr 1 subconstraints Nothing))
        -- (_, bestFinal) <- wt (\ thr -> async (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch 1 thr subconstraints))
        let finalNetwork = appendNetwork dbn (uncurry mkTNetwork (fst bestFinal))
        -- putStrLnTL $ printf "FINAL SCORE %s" (show $ scoreFinal)
        return finalNetwork
      evaluateCompleteNetwork completeNetwork = do
        mctsCount <- readIORef mctsCountVar
        winpct <- evaluateNetworkParams completeNetwork evalCount mctsCount
        when (winpct >= veryGoodScore) (do
                                        dumpCompleteNetwork completeNetwork winpct
                                        modifyIORef mctsCountVar (+mctsIncrement)
                                        c <- readIORef mctsCountVar
                                        printTL ("exp8::increased mctsCountVar to:", c)
                                        extendedEvaluation completeNetwork
                                      )
        return (winpct::Double)
      extendedEvaluation completeNetwork = do
        printTL "exp8::start extended evaluation"
        mctsCount <- readIORef mctsCountVar
        let counts = (map (mctsCount*) [1..5])
        wins <- mapM (evaluateNetworkParams completeNetwork evalCount) counts
        printTL ("exp8::extendedEvaluation", counts, wins)
      dumpCompleteNetwork net winpct = do
        printTL "exp8::dumpCompleteNetwork"
        printTL ("winpct",winpct)
        printTL "=======START======="
        printTL net
        printTL "========END========"
      dumpConstraintMapWeightsOnly = do
        printTL "exp8::constraint weight map dump"
        printTL "=======START======="
        constraintsNow <- ((sort . map (\(k,(p,c)) -> (p,c)) . Map.toList) <$> readIORef constraintMap) -- todo: make it prettier.
        mapM_ print (zip constraintsNow [1..]) 
        printTL "========END========"
      dumpConstraintMap = do
        printTL "exp8::constraint whole map dump"
        printTL "=======START======="
        constraintsNow <- ((sortBy (comparing (\(k,(p,c)) -> negate p)) . Map.toList) <$> readIORef constraintMap) -- todo: make it prettier.
        mapM_ print (zip constraintsNow [1..]) 
        printTL "========END========"

  forM_ [1..] $ \ bigStep -> do
    printTL ("exp8::bigStep", bigStep)
    sublist <- pickSublistWeighted
    updateScores sublist =<< evaluateCompleteNetwork =<< trainInterpretingLayer sublist
    when (bigStep `mod` bigStepDumpCount == 0) dumpConstraintMapWeightsOnly
    -- when (bigStep `mod` bigStepDumpCount == 0) dumpConstraintMap

showExperimentSource :: String
showExperimentSource = $(quoteThisFile)
