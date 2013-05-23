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
import System.Directory
import System.FilePath

useCachedDBN = False
searchTimeout = 5 # Minute
searchTimeoutMulti = 30 # Second
dbnGameCount = 250000
dbnGameProb = 0.07
dbnMatlabOpts = Just (def {dbnSizes = [1000], numEpochs = 5, implementation = Matlab})
constraintSource = CS_Gameplay playerUseCoinstraints gameplayConstraints'0
playerUseCoinstraints = 1500
allowedBad = round $ 0.1 * fromIntegral playerUseCoinstraints
workSetSize = 1000
singleNeuronTarget = 0.7
localSearch = 0.003

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
  forM_ [1..2] $ \ _attempt -> do
--  foreverUntilFileChanged "src/gg-exp5.hs" $ do
    threads <- getNumCapabilities
    scoredNeurons <- multiNeuronMinimalGAReprSearch threads allowedBad workSetSize searchTimeoutMulti singleNeuronTarget constraintsPacked
    let neurons = map fst scoredNeurons
        newLayer = mkLayer (neurons ++ mkBypass (getNeuronSize (head neurons)))
        dbnBigger = appendNetwork dbn newLayer
        constraintsPackedBigger = map (packConstraint dbnBigger) $ concatMap (uncurry generateConstraintsSimpleAll) constraints

    printTL ("dbnBigger", dbnBigger)
    printTL ("newLayer", newLayer, length neurons)

    bestRef <- newIORef (undefined, neginf)
    let wt thr'act = waitAnyCancel =<< withTimeout bestRef searchTimeout (mapM thr'act [1..threads])
    (_, _bestGA) <- wt (\ thr -> async (singleNeuronMinimalGAReprSearch (searchCB bestRef) thr 1 constraintsPackedBigger Nothing))
    (_, bestFinal) <- wt (\ thr -> async (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch 1 thr constraintsPackedBigger))

    let finalNetwork = appendNetwork dbn (uncurry mkTNetwork (fst bestFinal))
        baseDir = takeDirectory fn

    rndStr <- getRandomFileName
    writeFile (baseDir </> "dbn-final-data-ggexp5-"++rndStr++".txt") $ show $ finalNetwork
    wins <- evaluateLL dbnBigger bestFinal
    writeFile (baseDir </> "dbn-final-info-ggexp5-"++rndStr++".txt") $ show $ (showExperimentConfig, ("wins",wins), ("bestFinal",bestFinal))

showExperimentConfig = show $
        (("useCachedDBN         ", useCachedDBN         ) 
        ,("searchTimeout        ", searchTimeout        ) 
        ,("searchTimeoutMulti   ", searchTimeoutMulti   ) 
        ,("dbnGameCount         ", dbnGameCount         ) 
        ,("dbnGameProb          ", dbnGameProb          ) 
        ,("dbnMatlabOpts        ", dbnMatlabOpts        ) 
        ,("constraintSource     ", constraintSource     ) 
        ,("playerUseCoinstraints", playerUseCoinstraints) 
        ,("allowedBad           ", allowedBad           ) 
        ,("workSetSize          ", workSetSize          ) 
        ,("singleNeuronTarget   ", singleNeuronTarget   ) 
        ,("localSearch          ", localSearch          ))
