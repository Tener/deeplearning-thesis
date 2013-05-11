{-# LANGUAGE FlexibleContexts #-}

module ConstraintsGeneric where

import Prelude hiding (putStr, putStrLn)

import GenericGame
import AgentGeneric
import MinimalNN
import NeuralNets (parseNetFromFile)
import ThreadLocal

import Numeric.Container (sumElements)
import Data.Packed.Vector (Vector)
import qualified Data.Packed.Vector as Vector

import Control.Monad
import Data.IORef
import System.Random.MWC
import Text.Printf

data Constraint g = CBetter !g !g -- ^ first state is better than the other. assumes this is P1 turn.
                  | CBetterAll !g ![g] -- ^ first state is better then ALL the others. assumes this is P1 turn.

instance Functor Constraint where
    fmap f (CBetter g1 g2) = CBetter (f g1) (f g2)
    fmap f (CBetterAll g gs) = CBetterAll (f g) (fmap f gs)

type SingleNeuron = ([[[Double]]], [[Double]])

-- | check if constraint is held under given evaluation function
checkConstraint :: (g -> Double) -> Constraint g -> Bool
checkConstraint f con = case con of 
                          CBetter b0 b1 -> f b0 > f b1
                          CBetterAll b bs -> let aux = f b in all (\b' -> aux > f b') bs

-- | score constraints, returns percentage true
scoreConstraints :: (g -> Double) -> [Constraint g] -> Double
scoreConstraints f cons = let len = length cons
                              true = length $ filter id (map (checkConstraint f) cons)
                          in (fromIntegral true) / (fromIntegral len)

-- | generate constraints using base game state and a best choice following that state
generateConstraintsSimple :: (Game2 g, Eq g) => g -> g -> [Constraint g]
generateConstraintsSimple g'base g'good = if g'good `elem` moves g'base P1
                                               then [CBetter g'good g | g <- moves g'base P1, g /= g'good]
                                               else error "generateConstraintsSimple: best move not possible"

-- | generate constraints with `generateConstraintsSimple` and `AgentMCTS`
generateConstraintsMCTS :: (Repr (GameRepr g), Game2 g) => Int -> g -> ThrLocIO (g,g)
generateConstraintsMCTS mcts'gameCoutn g0 = do
  ag <- mkAgent mcts'gameCoutn :: ThrLocIO AgentMCTS
  generateConstraintsMCTS' ag g0

-- | generate constraints with `generateConstraintsSimple` and `AgentMCTS`
generateConstraintsMCTS' :: (Repr (GameRepr g), Game2 g) => AgentMCTS -> g -> ThrLocIO (g,g)
generateConstraintsMCTS' ag g0 = do
  g1 <- applyAgent ag g0 P1
  return (g0,g1)

generateConstraintsGameplay :: (Game2 g) => Player2 -> [g] -> [(g,g)]
generateConstraintsGameplay pl gameplay = let dropEven (o:_:rest) = o : dropEven rest
                                              dropEven [] = []
                                              dropEven [x] = [x]

                                              pairs = zip gameplay (tail gameplay)
                                          in
                                          case pl of
                                            P1 -> dropEven $ pairs
                                            P2 -> dropEven $ tail pairs

singleNeuronRandomReprSearch :: ((SingleNeuron, Double, IO ()) -> IO a) -- ^ callback called for new best score 
                             -> Double                                  -- ^ target value for score
                             -> Int                                     -- ^ thread number
                             -> [Constraint (Vector Double)]            -- ^ a list of good moves: @[(first state, second state)]@
                             -> ThrLocIO (SingleNeuron, Double)         -- ^ @(best'neuron, best'score)@ pair

singleNeuronRandomReprSearch newBest target thrnum constraints = do
  rgen <- withSystemRandom $ asGenIO $ return
  let lastLayerSize :: Int
      lastLayerSize = case head constraints of
                        CBetter c _ -> length (Vector.toList c)
                        CBetterAll c _ -> length (Vector.toList c)
      
      randomNeuron :: IO ([[[Double]]], [[Double]])
      randomNeuron = do
        (weights') <- (replicateM lastLayerSize (uniformR (-1,1) rgen))
        let bias = 0
        return ([[weights']],[[bias]])

      go (best'neuron,best'score) | best'score >= target = return (best'neuron,best'score)
                                  | otherwise = do
         neuron <- randomNeuron
         let score = scoreConstraints (sumElements . computeTNetworkSigmoid (uncurry mkTNetwork neuron)) constraints
         if score > best'score 
          then do
            let action = do
                             putStrLnTL (printf "[%d] NEURON %s" thrnum (show neuron))
                             let ccount = length constraints
                             putStrLnTL (printf "[%d] SCORE %f (cnum=%d, bad=%d)" thrnum score ccount (round $ fromIntegral ccount * (1-score) :: Int))
            void (newBest (neuron,score,action))
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,neginf)


singleNeuronRandomSearch :: (Eq g, Repr (GameRepr g), Game2 g) =>
                            ((SingleNeuron, Double, IO ()) -> IO a) -- ^ callback called for new best score 
                            -> Double                               -- ^ target value for score
                            -> Int                                  -- ^ thread number
                            -> FilePath                             -- ^ file with NN to read
                            -> [(g, g)]                             -- ^ a list of good moves: @[(first state, second state)]@
                            -> ThrLocIO (SingleNeuron, Double)      -- ^ @(best'neuron, best'score)@ pair

singleNeuronRandomSearch newBest target thrnum filename good'moves = do
  rgen <- withSystemRandom $ asGenIO $ return
  (dbn,sizes) <- parseNetFromFile `fmap` (readFile filename)
  let lastLayerSize :: Int
      lastLayerSize = last sizes
      
      randomNeuron :: IO ([[[Double]]], [[Double]])
      randomNeuron = do
        (weights') <- (replicateM lastLayerSize (uniformR (-1,1) rgen))
        let bias = 0
        return ([[weights']],[[bias]])

      constraints = concat $ map (uncurry generateConstraintsSimple) good'moves

      go (best'neuron,best'score) | best'score >= target = return (best'neuron,best'score)
                                  | otherwise = do
         neuron <- randomNeuron
         let score = scoreNeuron neuron dbn constraints
         if score > best'score 
          then do
            let action = do
                             putStrLnTL (printf "[%d] NEURON %s" thrnum (show neuron))
                             let ccount = length constraints
                             putStrLnTL (printf "[%d] SCORE %f (cnum=%d, bad=%d)" thrnum score ccount (round $ fromIntegral ccount * (1-score) :: Int))
            void (newBest (neuron,score,action))
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,neginf)

-- | simplified and possibly faster version of singleNeuronLocalSearch. constraints need to have preapplied game and toRepr functions as well as response from DBN.
singleNeuronLocalReprSearch :: ((SingleNeuron, Double, IO ()) -> IO ()) -- ^ callback called for new best neuron
                            -> IORef (SingleNeuron,Double)              -- ^ best neuron IORef. **WARNING**: make sure to update this IORef with values passed to callback.
                            -> Double                                   -- ^ base local search range
                            -> Double                                   -- ^ target value
                            -> Int                                      -- ^ thread num (affects effective search range)
                            -> [Constraint (Vector Double)]             -- ^ a list of constraints
                            -> ThrLocIO (SingleNeuron, Double)          -- ^ @(best'neuron, best'score)@ pair
singleNeuronLocalReprSearch newBest bestNeuronRef localSearchRange target thrnum constraints = do
  rgen <- withSystemRandom $ asGenIO $ return
  let lastLayerSize :: Int
      lastLayerSize = case head constraints of
                        CBetter c _ -> length (Vector.toList c)
                        CBetterAll c _ -> length (Vector.toList c)
                      
      randomNeuron :: IO ([[[Double]]], [[Double]])
      randomNeuron = do
        (([[weights'best]],_bias),_score) <- readIORef bestNeuronRef
        
        let actualRange = (fromIntegral thrnum) * localSearchRange
 
        weights' <- (replicateM lastLayerSize (uniformR (1-actualRange,1+actualRange) rgen))
        let bias = 0
        return ([[zipWith (*) weights'best weights']],[[bias]])
 
  let go (best'neuron,best'score) | best'score >= target = return (best'neuron,best'score)
                                  | otherwise = do
         neuron <- randomNeuron
         let score = scoreConstraints (sumElements . computeTNetworkSigmoid (uncurry mkTNetwork neuron)) constraints
         if score > best'score 
          then do
            let action = do
                             putStrLnTL (printf "[%d] LOCAL NEURON %s" thrnum (show neuron))
                             let ccount = length constraints
                             putStrLnTL (printf "[%d] LOCAL SCORE %f (cnum=%d, bad=%d)" thrnum score ccount (round $ fromIntegral ccount * (1-score) :: Int) )
            newBest (neuron,score,action)
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,neginf)

singleNeuronLocalSearch :: (Eq g, Repr (GameRepr g), Game2 g) =>
                           ((SingleNeuron, Double, IO ()) -> IO ()) -- ^ callback called for new best neuron
                        -> IORef (SingleNeuron,Double)              -- ^ best neuron IORef. **WARNING**: make sure to update this IORef with values passed to callback.
                        -> Double                                   -- ^ base local search range
                        -> Double                                   -- ^ target value
                        -> Int                                      -- ^ thread num (affects effective search range)
                        -> FilePath                                 -- ^ filename with NN
                        -> [(g, g)]                                 -- ^ a list of good moves: @[(first state, second state)]@
                        -> ThrLocIO (SingleNeuron, Double)          -- ^ @(best'neuron, best'score)@ pair
 
singleNeuronLocalSearch newBest bestNeuronRef localSearchRange target thrnum filenameNN good'moves = do
  rgen <- withSystemRandom $ asGenIO $ return
  (dbn,sizes) <- parseNetFromFile `fmap` (readFile filenameNN)
  let lastLayerSize :: Int
      lastLayerSize = last sizes
      
      randomNeuron :: IO ([[[Double]]], [[Double]])
      randomNeuron = do
        (([[weights'best]],_bias),_score) <- readIORef bestNeuronRef
        
        let actualRange = (fromIntegral thrnum) * localSearchRange
 
        weights' <- (replicateM lastLayerSize (uniformR (1-actualRange,1+actualRange) rgen))
        let bias = 0
        return ([[zipWith (*) weights'best weights']],[[bias]])
 
  let constraints = concat $ map (uncurry generateConstraintsSimple) good'moves
 
      go (best'neuron,best'score) | best'score >= target = return (best'neuron,best'score)
                                  | otherwise = do
         neuron <- randomNeuron
         let score = scoreNeuron neuron dbn constraints
         if score > best'score 
          then do
            let action = do
                             putStrLnTL (printf "[%d] LOCAL NEURON %s" thrnum (show neuron))
                             let ccount = length constraints
                             putStrLnTL (printf "[%d] LOCAL SCORE %f (cnum=%d, bad=%d)" thrnum score ccount (round $ fromIntegral ccount * (1-score) :: Int) )
            newBest (neuron,score,action)
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,neginf)

-- helpers 

neginf :: Double
neginf = negate (1/0)

scoreNeuron :: (Repr (GameRepr g), Game2 g) => SingleNeuron -> TNetwork -> [Constraint g] -> Double
scoreNeuron n dbn constraints = scoreConstraints (evalNeuronGame n dbn) constraints

-- | fixme: merge with AgentSimpleLL code
evalNeuronGame :: (Repr (GameRepr g), Game2 g) => SingleNeuron -> TNetwork -> g -> Double
evalNeuronGame neuron dbn game = 
    let final = Vector.toList $ computeTNetworkSigmoid neuronTNet 
                              $ computeTNetworkSigmoid dbn 
                              $ reprToNN $ toRepr game
        neuronTNet :: TNetwork
        neuronTNet = uncurry mkTNetwork neuron
       in
    case final of
      [x] -> x
      [] -> error "No values returned. What?!"
      _ -> error "Too many values returned. Last layer too big."
