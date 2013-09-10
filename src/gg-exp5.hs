{-# LANGUAGE Rank2Types, OverloadedStrings #-}

module Main where

import Prelude hiding (putStr, putStrLn)

import ConstraintsGA
import ConstraintsGeneric
import GenericGameExperiments
import Matlab
import MinimalNN
import NeuralNets
import ThreadLocal

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Default
import Data.IORef
import Data.Timeout
import System.FilePath

useCachedDBN = False
searchTimeout = 1 # Minute
searchTimeoutMulti = 10 # Second
dbnGameCount = 100000
dbnGameProb = 0.01
dbnMatlabOpts = Just (def {dbnSizes = [750], numEpochs = 15, implementation = Matlab})
constraintSource = CS_Gameplay playerUseCoinstraints gameplayConstraints'0
playerUseCoinstraints = 1000
constraintsStage2Count = 500
allowedBad = round $ 0.10 * fromIntegral workSetSize
workSetSize = 100
singleNeuronTarget = 0.9
localSearch = 0.003
attemptsCount = 4

mkLayer :: [SingleNeuron] -> TNetwork
mkLayer neurons = let ws = [map ((\ [[w]] -> w ) . fst) neurons]
                      bs = [map ((\ [[b]] -> b ) . snd) neurons]
                  in mkTNetwork ws bs

getNeuronSize :: SingleNeuron -> Int
getNeuronSize ([[w]],_) = length w

main :: IO ()
main = runThrLocMainIO $ do
  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  -- fn <- mutateRealGamesTrainNetwork someGame allGameRecords dbnGameCount (dbnGameProb/5) 0.5 dbnMatlabOpts
  -- let fn = "tmp-data/iybjioktvbdgmjocdtow/dbn.txt"
  -- let fn = "tmp-data/iwssqgpqsryvajvoerqi/dbn.txt"
  printTL ("DBN FN=",fn)
  dbn <- getDBNFile fn

  printTL "Constraint generation"
  constraints <- getConstraints constraintSource
  let constraintsPacked = map (packConstraint dbn) $ concatMap (uncurry generateConstraintsSimple) constraints
  printTL ("Total coinstraint count", length constraintsPacked)
  printTL "Evaluating packed constraints..."
  print $ head constraintsPacked
--  (constraintsPacked `using` parList rdeepseq) `deepseq` printTL "Done."
  printTL "Perform multiNeuronMinimalGAReprSearch"
  threads <- getNumCapabilities
  scoredNeurons <- multiNeuronMinimalGAReprSearch threads allowedBad workSetSize searchTimeoutMulti singleNeuronTarget constraintsPacked

  printTL "Do few times: train last layer network & evaluate"
  forM_ [1..attemptsCount] $ \ attempt -> do
    let neurons = map fst scoredNeurons
        -- newLayer = mkLayer (neurons ++ mkBypass (getNeuronSize (head neurons)))
        newLayer = mkLayer neurons
        dbnBigger = appendNetwork dbn newLayer
        constraintsPackedBigger = take constraintsStage2Count $ 
                                  map (packConstraint dbnBigger) $
                                  concatMap (uncurry generateConstraintsSimple) constraints

    printTL ("dbnBigger", dbnBigger)
    printTL ("newLayer", newLayer, length neurons)

    bestRef <- newIORef (undefined, neginf)
    let wt thr'act = waitAnyCancel =<< withTimeout bestRef searchTimeout (mapM thr'act [1..threads])
    (_, _bestGA) <- wt (\ thr -> async (singleNeuronMinimalGAReprSearch (searchCB bestRef) thr 1 constraintsPackedBigger Nothing))
    (_, bestFinal) <- wt (\ thr -> async (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch 1 thr constraintsPackedBigger))

    let finalNetwork = appendNetwork dbnBigger (uncurry mkTNetwork (fst bestFinal))
        baseDir = takeDirectory fn

    rndStr <- getRandomFileName
    writeFile (baseDir </> "dbn-final-data-ggexp5-"++rndStr++".txt") $ show $ finalNetwork
    wins <- evaluateLL finalNetwork (snd bestFinal)
    writeFile (baseDir </> "dbn-final-info-ggexp5-"++rndStr++".txt") $ showExperimentConfig wins bestFinal

showExperimentConfig wins bestFinal = unlines $
        ["wins                 " ++ (show wins                 )
        ,"bestFinal            " ++ (show bestFinal            )
        ,"useCachedDBN         " ++ (show useCachedDBN         ) 
        ,"searchTimeout        " ++ (show searchTimeout        ) 
        ,"searchTimeoutMulti   " ++ (show searchTimeoutMulti   ) 
        ,"dbnGameCount         " ++ (show dbnGameCount         ) 
        ,"dbnGameProb          " ++ (show dbnGameProb          ) 
        ,"dbnMatlabOpts        " ++ (show dbnMatlabOpts        ) 
        ,"constraintSource     " ++ (show constraintSource     ) 
        ,"playerUseCoinstraints" ++ (show playerUseCoinstraints) 
        ,"allowedBad           " ++ (show allowedBad           ) 
        ,"workSetSize          " ++ (show workSetSize          ) 
        ,"singleNeuronTarget   " ++ (show singleNeuronTarget   ) 
        ,"localSearch          " ++ (show localSearch          ) 
        ]
