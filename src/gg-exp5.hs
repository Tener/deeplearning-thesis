{-# LANGUAGE Rank2Types, OverloadedStrings #-}

module Main where

import Prelude hiding (putStr, putStrLn)

import ConstraintsGA
import ConstraintsGeneric
import GenericGameExperiments
import Matlab
import MinimalNN
import ThreadLocal

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Default
import Data.IORef
import Data.Timeout

useCachedDBN = False
constraintSource = CS_Gameplay playerUseCoinstraints gameplayConstraints'0
searchTimeout = 1 # Minute
searchTimeoutMulti = 1 # Minute
dbnGameCount = 25000
dbnGameProb = 0.01
dbnMatlabOpts = Just (def {dbnSizes = [250], numEpochs = 5, implementation = Matlab})
playerUseCoinstraints = 1000
allowedBad = round $ 0.1 * fromIntegral playerUseCoinstraints

singleNeuronTarget = 1.0
localSearch = 0.003

mkLayer :: [SingleNeuron] -> TNetwork
mkLayer neurons = let ws = [map ((\ [[w]] -> w ) . fst) neurons]
                      bs = [map ((\ [[b]] -> b ) . snd) neurons]
                  in mkTNetwork ws bs

main :: IO ()
main = runThrLocMainIO $ do
  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  -- let fn = "tmp-data/iybjioktvbdgmjocdtow/dbn.txt"
  printTL ("DBN FN=",fn)
  dbn <- getDBNFile fn

  printTL "Constraint generation"
  constraints <- getConstraints constraintSource
  let constraintsPacked = map (packConstraint dbn) $ concatMap (uncurry generateConstraintsSimple) constraints
  printTL ("Total coinstraint count", length constraintsPacked)
  printTL "Evaluating packed constraints..."
  print $ head constraintsPacked
  (constraintsPacked `using` parList rdeepseq) `deepseq` printTL "Done."

  printTL "forever train last layer network & evaluate"
--  forM_ [1..5] $ \ _attempt -> do
  foreverUntilFileChanged "src/gg-exp5.hs" $ do
    threads <- getNumCapabilities
    scoredNeurons <- multiNeuronMinimalGAReprSearch threads allowedBad searchTimeoutMulti singleNeuronTarget constraintsPacked
    let neurons = map fst scoredNeurons
        newLayer = mkLayer neurons
        dbnBigger = appendNetwork dbn newLayer
        constraintsPackedBigger = map (packConstraint dbnBigger) $ concatMap (uncurry generateConstraintsSimpleAll) constraints

    printTL ("dbnBigger", dbnBigger)
    printTL ("newLayer", newLayer, length neurons)

    bestRef <- newIORef (undefined, neginf)
    let wt thr'act = waitAnyCancel =<< withTimeout bestRef searchTimeout (mapM thr'act [1..threads])
    (_, bestGA) <- wt (\ thr -> async (singleNeuronMinimalGAReprSearch (searchCB bestRef) thr 1 constraintsPackedBigger Nothing))
    (_, bestFinal) <- wt (\ thr -> async (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch 1 thr constraintsPackedBigger))
    evaluateLL dbnBigger bestFinal

