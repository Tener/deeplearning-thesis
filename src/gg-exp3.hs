{-# LANGUAGE ImplicitParams, Rank2Types, BangPatterns, OverloadedStrings #-}

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
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Default
import Data.IORef
import Data.Timeout

useCachedDBN = False || nullDBN
nullDBN = False
constraintSource = CS_Gameplay playerUseCoinstraints
searchTimeout = 5 # Minute
dbnGameCount = 1500000
dbnGameProb = 0.1
dbnMatlabOpts = Just (def {dbnSizes = [750], numEpochs = 5, implementation = Matlab})
playerUseCoinstraints = 1500

-- thresholds for global & local search, local search radius
thrG = 1          
thrL = 1          
localSearch = 0.003

main :: IO ()
main = runThrLocMainIO $ do
  printTL "Constraint generation"
  constraints <- getConstraints constraintSource

  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  printTL ("DBN FN=",fn)
  printTL "forever train last layer network & evaluate"
  foreverUntilFileChanged "src/gg-exp3.hs" $ do
    threads <- getNumCapabilities
    bestRef <- newIORef (undefined, neginf)
    let getDBNNull = return (mkTNetwork [] [])
    dbn <- if nullDBN then getDBNNull else getDBNFile fn
    let constraintsPacked = map (packConstraint dbn) $ concatMap (uncurry generateConstraintsSimpleAll) constraints
        asyncTL thr as = let old = ?thrLoc in
                         let ?thrLoc = old { tl_ident = show thr } in
                         async as

    printTL ("Total coinstraint count", length constraintsPacked)
    printTL ("Evaluating packed constraints...")
    print $ head constraintsPacked
    (constraintsPacked `using` parList rdeepseq) `deepseq` printTL ("Done.")
    let wt thr'act = waitAnyCancel =<< withTimeout bestRef searchTimeout (mapM thr'act [1..threads])

    (_, _best2) <- wt (\ thr -> asyncTL thr (singleNeuronMinimalGAReprSearch (searchCB bestRef) thr thrL constraintsPacked Nothing))
    (_, _best) <- wt (\ thr -> asyncTL thr (singleNeuronRandomReprSearch (searchCB bestRef) thrG thr constraintsPacked))
    (_, bestFinal) <- wt (\ thr -> asyncTL thr (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch thrL (thr*2) constraintsPacked))

    evaluateLL dbn bestFinal
