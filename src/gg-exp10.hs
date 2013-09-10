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

type DBN = TNetwork

-- | this is our working element. first is DBN, then single layer of interpeters, finally summed by (virtual) neuron at the end with weights tied to every neuron.
--   the @g@ type param fixes game type, @ag@ type param fixes agent type.
data EvolNet ag g = EvolNet DBN [(NN1, Double)] deriving (Show,Eq,Ord,Read)
type MyEntType = EvolNet (AgentSimple TNetwork) Breakthrough

useCachedDBN = False
searchTimeout = 10 # Second
dbnGameCount = 100000
dbnGameProb = 0.01
dbnMatlabOpts = Just (def {dbnSizes = [50], numEpochs = 5, implementation = Matlab})
constraintSource = CS_Gameplay playerUseCoinstraints gameplayConstraints'0
playerUseCoinstraints = 100
evalCount = 12
mctsCount = 10 :: Int
constraintSetSize = 10
ecPopulationSizeP = 50
ecKillCountP = ecPopulationSizeP - (ecPopulationSizeP `div` 5)
randomGamesCount :: (Num a) => a
randomGamesCount = 10
randomGamesProb :: Float
randomGamesProb = 0.1
initialNeuronCount = 100
nn1'mutation'range :: (Num a) => (a,a)
nn1'mutation'range = (0,1)
mctsLevel = 50

main :: IO ()
main = runThrLocMainIO $ do
  printTL "exp10::start new run"
  printTL "source code for this experiment: "
  putStrLnTL showExperimentSource
  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  printTL ("DBN FN=",fn)
  (dbn,dbnLayerSizes) <- parseNetFromFile <$> readFile fn
  let dbnLastLayerSize = last dbnLayerSizes
  threads <- getNumCapabilities
  gen <- mkGenIO

  -- score reporting
  let reportScores (esBest -> (score, ent)) = do
        printTL ("exp10::evolutionNewStep::best.score",score)
        printTL ("exp10::evolutionNewStep::best.ent", (ent :: MyEntType))
        benchmarkEntity ent
        
  -- evolution params
  let entParams = (dbn,initialNeuronCount)
      workParams = (gen,dbnLastLayerSize)
      scoreParams = ([],[]) -- scoreParams = ((readIORef agentsRef),(readIORef gamesRef))
      callbackNewStep esParams = do
        printTL ("exp10::callbackNewStep")
        -- populateGamesRef
        -- populateAgentsRef (map snd $ (esPopulation esParams ++ esArchive esParams))
        reportScores esParams
        return True
      evOpts = def { ecPopulationSize = ecPopulationSizeP
                   , ecArchiveSize = 0
                   , ecKillCount = ecKillCountP
                   , ecCallbackNewStep = callbackNewStep
                   }

  printTL "exp10::evolve"
  results <- evolve evOpts entParams workParams scoreParams
  mapM_ (\ (score,ent) -> do
            printTL ("exp10::score",score)
            printTL ("exp10::ent",(ent :: MyEntType))
            ) results
  printTL "exp10::finished"

benchmarkEntity :: MyEntType -> ThrLocIO ()
benchmarkEntity ent@(EvolNet dbn lst) = do
  printTL "exp10::benchmarkEntity"
  printTL ent
  myAgent <- mkAgentEvolNet (ent :: MyEntType)
  agRnd <- mkAgent ()
  agMCTS <- mkAgent mctsLevel
  wcRND <- reportWinCount 100 myAgent (agRnd :: AgentRandom) P1
  printTL ("exp10::win count random", wcRND)
  wcMCTS <- reportWinCount 10 myAgent (agMCTS :: AgentMCTS) P1
  printTL ("exp10::win count MCTS", wcMCTS)


showExperimentSource :: String
showExperimentSource = $(quoteThisFile)

mkAgentEvolNet :: (EvolNet ag g) -> IO (AgentSimple TNetwork)
mkAgentEvolNet (EvolNet dbn lst) = runThrLocMainIO (mkAgent (appendNetwork dbn (appendNetwork interpretingLayer gatherNeuron)))
    where
      -- int. layer
      neurons = map (getNeuron . fst) lst
      (weights, biases) = unzip neurons
      interpretingLayer = mkTNetwork [concat $ concat weights] [concat $ concat biases]
      -- gath. neuron
      gn'weigths = map snd lst
      gatherNeuron = uncurry mkTNetwork . getNeuron . mkSingleNeuron $ gn'weigths

calcEntitySize :: (EvolNet ag g) -> Int
calcEntitySize (EvolNet _ lst) = length lst

newtype Double' = Double' { fromD' :: Double } deriving (Ord,Eq)
instance Show Double' where
  show d = printf "%2.5f" (fromD' d)

instance (Agent2 ag, ag ~ (AgentSimple TNetwork), g ~ Breakthrough) => MinimalGA (EvolNet ag g) where
  type Score (EvolNet ag g) = (Double',Int)
  -- | other networks to compare against, set of game states
  type ScoreDataset (EvolNet ag g) = ([ag],[g])
  -- | dbn, new neuron count
  type EntityParams (EvolNet ag g) = (DBN, Int)
  -- | DBN last layer size
  type WorkParams (EvolNet ag g) = (GenIO,Int)

  newEntity (gen,dbnSize) (dbn,initialLayerSize) = do
    nns <- replicateM initialLayerSize (newEntity (gen,dbnSize,nn1'mutation'range) dbnSize)
    weights <- replicateM initialLayerSize (uniformR (-1,1) gen)
    return (EvolNet dbn (zip nns weights))

  crossover (gen,_) (EvolNet dbn neurons1) (EvolNet _ neurons2) = do
    let neurons = neurons1 ++ neurons2
    choices <- replicateM (length neurons) (uniform gen)
    let chosenNeurons = catMaybes (zipWith boolToMaybe choices neurons)
    return (EvolNet dbn chosenNeurons)
    
  mutation (gen,dbnSize) mutForce (EvolNet dbn neurons) = do
    let params = (gen,dbnSize,nn1'mutation'range)
    neurons' <- mapM (mutation params mutForce) (map fst neurons)
    let weights = map snd neurons
        single = mkSingleNeuron weights
    single' <- mutation params mutForce single
    let weights' = concat . concat . fst . getNeuron $ single'
        neurons'new = zip neurons' weights'
    return (EvolNet dbn neurons'new)
  
  scoreEntity (agents, games) ent = do
    caps <- getNumCapabilities
    --
    entAgents <- replicateM caps (mkAgentEvolNet ent)
    let cb = GameDriverCallback (\ _ -> return ()) (\ _ _ -> return True)
        work = zip entAgents (transpose $ splitEvery caps [ \ thisAg -> driverG2 g (thisAg :: AgentSimple TNetwork) otherAg cb | otherAg <- agents, g <- games ])
    states <- concat <$> mapConcurrently (\ (agent, tasks) -> mapM (\ t -> t agent) tasks) work
    -- game score
    let wins = length $ filter (==(Just P1)) $ map winner states
        totalGames = max (length states) 1 -- hack, but we dont want divide by zero.
        score = Double' (1 - ((fromIntegral wins) / (fromIntegral totalGames)))
    -- size score
    let size = calcEntitySize ent
        final = (score, size)
    printTL ("exp10::final entity score",final)
    return final

  scorePopulation dataset entities = do
    -- prepare games
    gamesRef <- newIORef []
    sampleRandomGamesCount randomGamesCount randomGamesProb (\g -> modifyIORef gamesRef (g:))
    games <- readIORef gamesRef
    -- prepare agents
    agents <- mapM mkAgentEvolNet entities
    -- calculate scores
    scores <- mapM (scoreEntity (agents, games)) entities
    return (zip scores entities)
