{-# LANGUAGE ViewPatterns, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
-- based on experiment 5 and 8, we try to find the best interpretation of output by making a tournament between constraint sets.
-- the evolution objects are constraint sets. we favor smaller sets so there is small penalty for them being large.
-- calculating set fitness is expensive so we do it only once but more-or-less precisely by using relatively large number of games.

module Main where

import Prelude hiding (putStr, putStrLn)

import ConstraintsGA
import ConstraintsGeneric
import GenericGameExperiments
import Matlab
import MinimalNN
import MinimalGA
import NeuralNets
import ThreadLocal
import Utils
import THUtils
import MyVectorType(Vector)

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
import System.Random.MWC

import qualified Data.Map as Map
import Data.List (sortBy,sort)
import Data.Ord (comparing)
import Data.Default

data ConstraintSet = ConstraintSet [(Constraint (Vector Double))] deriving (Eq,Ord,Show)
type DBN = TNetwork

useCachedDBN = False
searchTimeout = 10 # Second
dbnGameCount = 100000
dbnGameProb = 0.1
dbnMatlabOpts = Just (def {dbnSizes = [75], numEpochs = 5, implementation = Matlab})
constraintSource = CS_Gameplay playerUseCoinstraints gameplayConstraints'0
playerUseCoinstraints = 100
evalCount = 12
mctsCount = 20 :: Int
constraintSetSize = 10
ecPopulationSizeP = 10
ecArchiveSizeP = 3

main :: IO ()
main = runThrLocMainIO $ do
  printTL "exp9::start new instance"
  printTL "source code for this experiment: "
  putStrLnTL showExperimentSource
  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  printTL ("DBN FN=",fn)
  dbn <- getDBNFile fn

  printTL "Constraint generation"
  constraints <- getConstraints constraintSource
  let constraintsPacked = map (packConstraint dbn) $ concatMap (uncurry generateConstraintsSimpleAll) constraints
  printTL ("Total coinstraint count", length constraintsPacked)
  printTL "Evaluating packed constraints..."
  print $ head constraintsPacked
  printTL "Constraints:"
  printTL constraints
  threads <- getNumCapabilities
  gen <- mkGenIO
  scoreCount <- newIORef 1

  let universe = (ConstraintSet constraintsPacked)
      entParams = (universe, constraintSetSize)
      workParams = (universe, gen)
      scorecb score = do
        cnt <- readIORef scoreCount
        printTL ("exp9::constraint set score", cnt, score)
        modifyIORef scoreCount (+1)
        return ()
      scoreDataset = (scorecb, dbn, threads, mctsCount)
      callbackNewStep (esBest -> (score, ent)) = do
        printTL ("exp9::evolutionNewStep::best.score",score)
        printTL ("exp9::evolutionNewStep::best.ent", (ent :: ConstraintSet))
        return True
      evOpts = def { ecPopulationSize = ecPopulationSizeP
                   , ecArchiveSize = ecArchiveSizeP
                   , ecCallbackNewStep = callbackNewStep
                   }
               
  results <- evolve evOpts entParams workParams scoreDataset
  mapM_ (\ (score,ent) -> do
            printTL ("score",score)
            printTL ("ent",(ent :: ConstraintSet))
            ) results

showExperimentSource :: String
showExperimentSource = $(quoteThisFile)

instance MinimalGA ConstraintSet where
    type Score ConstraintSet = Double
    -- | random gen, thread count, mcts count
    type ScoreDataset ConstraintSet = ((Double -> IO ()), DBN, Int, Int)
    -- | element source, basic subset size
    type EntityParams ConstraintSet = (ConstraintSet,Int)
    -- | universe element source, random gen
    type WorkParams ConstraintSet = (ConstraintSet,GenIO)

    newEntity (_,rgen) ((ConstraintSet constraints), count) = (ConstraintSet . nubSort . sort . take count) <$> (shuffleGen rgen constraints)
    crossover wp (ConstraintSet cs1) (ConstraintSet cs2) = do
        let size = (length cs1) + (length cs2)
            count = size `div` 2
            constraints = cs1 ++ cs2
        newEntity wp ((ConstraintSet constraints),count)
        
    mutation wp@((ConstraintSet constraintsAll),rgen) mutChance (ConstraintSet cs) = do
      let replaceChance = 0.5 :: Double
          dropOrReplace = do
            b <- uniform rgen
            if (b < replaceChance)
              then do
                new <- pickList rgen constraintsAll
                rest <- dropOrReplace
                return (new:rest)
              else return []

      newlist <- concat <$>
        mapM (\ const -> do
               val <- uniform rgen
               if (val > mutChance)
                 then return [const]
                 else dropOrReplace
           ) cs
      return (ConstraintSet (nubSort newlist))

    scoreEntity (callback, dbn, threads, mctsCount') (ConstraintSet cs) = do
      let trainInterpretingLayer subconstraints = do
            bestRef <- newIORef (undefined, neginf)
            let wt thr'act = waitAnyCancel =<< withTimeout bestRef searchTimeout (mapM thr'act [1..threads])
            (_, bestFinal) <- wt (\ thr -> async (singleNeuronMinimalGAReprSearch (searchCB bestRef) thr 1 subconstraints Nothing))
            let finalNetwork = appendNetwork dbn (uncurry mkTNetwork (fst bestFinal))
            return finalNetwork
          evaluateCompleteNetwork completeNetwork = do
            winpct <- evaluateNetworkParams completeNetwork evalCount mctsCount'
            return (winpct::Double)
      score <- evaluateCompleteNetwork =<< trainInterpretingLayer cs
      callback score
      return (negate score)
