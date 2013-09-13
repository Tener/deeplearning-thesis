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

useCachedDBN = False
-- searchTimeout = 10 # Second
dbnGameCount = 100000
dbnGameProb = 0.1
dbnMatlabOpts = Just (def {dbnSizes = [25], numEpochs = 5, implementation = Matlab})
-- constraintSource = CS_Gameplay playerUseCoinstraints gameplayConstraints'0
-- playerUseCoinstraints = 100
-- evalCount = 12
mctsCount = 10 :: Int
-- constraintSetSize = 20
ecPopulationSizeP = 20
ecKillCountP = ecPopulationSizeP `div` 2
randomGamesCount :: (Num a) => a
randomGamesCount = 50
randomGamesProb :: Float
randomGamesProb = 0.01
initialNeuronCount = 50
nn1'mutation'range :: (Num a) => (a,a)
nn1'mutation'range = (0,1)
mctsLevel = 75

main :: IO ()
main = runThrLocMainIO $ do
  printTL "exp10::start new run"
  printTL "source code for this experiment: "
  putStrLnTL showExperimentSource
  printTL "DBN read/train"
  fn <- getDBNCachedOrNew useCachedDBN dbnGameCount dbnGameProb dbnMatlabOpts
  printTL ("DBN FN=",fn)
  (dbn,dbnLayerSizes) <- parseNetFromFile <$> readFile fn
--  (dbn,dbnLayerSizes) <- return ((mkTNetwork [] []), [192])
  let dbnLastLayerSize = last dbnLayerSizes
  threads <- getNumCapabilities
  gen <- mkGenIO

  -- score reporting
  let reportScores (esBest -> (score, ent)) = do
        printTL ("exp10::evolutionNewStep::best.score",score)
        printTL ("exp10::evolutionNewStep::best.ent")
        printTL (ent :: MyEntType)
        -- (AgentGameTree dbn _) <- mkAgentEvolNet ent
        (AgentSimple dbn) <- mkAgentEvolNet ent
        printTL ("exp10::evolutionNewStep::best.dbn::start")
        printTL dbn
        printTL ("exp10::evolutionNewStep::best.dbn::end")
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
  -- threads <- getNumCapabilities
  -- setNumCapabilities 1
  printTL "exp10::benchmarkEntity"
  printTL ent
  myAgent <- mkAgentEvolNet (ent :: MyEntType)
  agRnd <- mkAgent ()
  agMCTS <- mkAgent mctsLevel
  wcRND <- reportWinCount 100 myAgent (agRnd :: AgentRandom) P1
  printTL ("exp10::win count random", wcRND)
  wcMCTS <- reportWinCount 10 myAgent (agMCTS :: AgentMCTS) P1
  printTL ("exp10::win count MCTS", wcMCTS)
  -- setNumCapabilities threads

showExperimentSource :: String
showExperimentSource = $(quoteThisFile)

mkAgentEvolNet :: (EvolNet ag g) -> IO (AgentSimple TNetwork) -- (AgentGameTree TNetwork) -- 
mkAgentEvolNet (EvolNet dbn lst) = runThrLocMainIO $ do
                                     mkAgent finalNetwork
                                     -- mkAgent (finalNetwork, 2)
    where
      -- int. layer
      neurons = map (getNeuron . fst) lst
      (weights, biases) = unzip neurons
      interpretingLayer = mkTNetwork [concat $ concat weights] [concat $ concat biases]
      -- gath. neuron
      gn'weigths = map snd lst
      gatherNeuron = uncurry mkTNetwork . getNeuron . mkSingleNeuron $ gn'weigths
      -- final network
      finalNetwork = (appendNetwork dbn (appendNetwork interpretingLayer gatherNeuron))

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
    error "exp10::scoreEntity is undefined for single entity"
--    caps <- getNumCapabilities
--    --
--    entAgents <- replicateM caps (mkAgentEvolNet ent)
--    let cb = GameDriverCallback (\ _ -> return ()) (\ _ _ -> return True)
--        work = zip entAgents (splitWorkCaps caps [ \ thisAg -> driverG2 g (thisAg :: AgentSimple TNetwork) otherAg cb | otherAg <- agents, g <- games ])
--    states <- concat <$> mapConcurrently (\ (agent, tasks) -> mapM (\ t -> t agent) tasks) work
--    -- game score
--    let wins = length $ filter (==(Just P1)) $ map winner states
--        totalGames = max (length states) 1 -- hack, but we dont want divide by zero.
--        score = Double' (1 - ((fromIntegral wins) / (fromIntegral totalGames)))
--    -- size score
--    let size = calcEntitySize ent
--        final = (score, size)
--    printTL ("exp10::final entity score",final)
--    return final

  scorePopulation _dataset entities = do
    -- prepare games
    gamesRef <- newIORef []
    sampleRandomGamesCount randomGamesCount randomGamesProb (\g -> modifyIORef gamesRef ((g::Breakthrough):))
    games <- readIORef gamesRef
    -- scores
    wins <- newMVar (Map.fromList (zip entities (cycle [0]))) -- win counts
    let addWin entity = modifyMVar_ wins ((return . addWinMap entity)$!)
        addWinMap entity mapping = Map.adjust (+1) entity mapping
        totalGames = 2 * (sum [ 1 | e1 <- entities, e1 /= (head entities), g <- games ])
        calcFinalScore mapping entity =
          case Map.lookup entity mapping of
            Nothing -> error "exp10::missing entity in map"
            Just wins -> do let score = Double' (1 - ((fromIntegral wins) / (fromIntegral totalGames)))
                                size = calcEntitySize entity
                                final = (score,size)
                            printTL ("exp10::final entity score",final)
                            return final

    -- run game pairs
    let runGame ent1 ent2 g = do
          ag1 <- mkAgentEvolNet ent1
          ag2 <- mkAgentEvolNet ent2
          let cb = GameDriverCallback (\ _ -> return ()) (\ _ _ -> return True)
          finished <- driverG2 g ag1 ag2 cb
          case winner finished of
            Nothing -> error "exp10::scorePopulation::finished game, no winner"
            Just P1 -> addWin ent1
            Just P2 -> addWin ent2
    runWorkThreadsProgress_ [ runGame e1 e2 g | e1 <- entities, e2 <- entities, e1 /= e2, g <- games ]

    -- calculate scores
    scoreMap <- takeMVar wins
    scores <- mapM (calcFinalScore scoreMap) entities
    return (zip scores entities)
