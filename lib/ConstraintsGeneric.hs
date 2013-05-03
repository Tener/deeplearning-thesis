{-# LANGUAGE FlexibleContexts #-}

module ConstraintsGeneric where

import GenericGame
import MinimalNN
import NeuralNets (parseNetFromFile)

import Control.Monad
import Data.IORef
import System.Random.MWC
import Text.Printf
import qualified Data.Packed.Vector as Vector

data Constraint g = CBetter g g -- ^ first state is better than the other. assumes this is P1 turn.
                  | CBetterAll g [g] -- ^ first state is better then ALL the others. assumes this is P1 turn.

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

generateConstraintsSimple :: (Game2 g, Eq g) => g -> g -> [Constraint g]
generateConstraintsSimple g'base g'good = if g'good `elem` moves g'base P1
                                               then [CBetter g'good g | g <- moves g'base P1, g /= g'good]
                                               else error "generateConstraintsSimple: best move not possible"


singleNeuronRandomSearch :: (Eq g, Repr (GameRepr g), Game2 g) =>
                            ((SingleNeuron, Double, IO ()) -> IO a) -- ^ callback called for new best score 
                            -> Double                               -- ^ target value for score
                            -> Int                                  -- ^ thread number
                            -> FilePath                             -- ^ file with NN to read
                            -> [(g, g)]                             -- ^ a list of good moves: @[(first state, second state)]@
                            -> IO (SingleNeuron, Double)            -- ^ @(best'neuron, best'score)@ pair

singleNeuronRandomSearch newBest target thrnum filename good'moves = do
  rgen <- withSystemRandom $ asGenIO $ return
  (dbn,sizes) <- parseNetFromFile `fmap` (readFile filename)
  print (dbn,sizes)
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
                             putStrLn (printf "[%d] NEURON %s" thrnum (show neuron))
                             putStrLn (printf "[%d] SCORE %f (cnum=%d)" thrnum score (length constraints))
            void (newBest (neuron,score,action))
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,neginf)

singleNeuronLocalSearch :: (Eq g, Repr (GameRepr g), Game2 g) =>
                           ((SingleNeuron, Double, IO ()) -> IO ()) -- ^ callback called for new best neuron
                        -> IORef (SingleNeuron,Double)              -- ^ best neuron IORef
                        -> Double                                   -- ^ base local search range
                        -> Double                                   -- ^ target value
                        -> Int                                      -- ^ thread num (affects effective search range)
                        -> FilePath                                 -- ^ filename with NN
                        -> [(g, g)]                                 -- ^ a list of good moves: @[(first state, second state)]@
                        -> IO (SingleNeuron, Double)                -- ^ @(best'neuron, best'score)@ pair
 
singleNeuronLocalSearch newBest bestNeuronRef localSearchRange target thrnum filenameNN good'moves = do
  gen <- withSystemRandom $ asGenIO $ return
  (dbn,sizes) <- parseNetFromFile `fmap` (readFile filenameNN)
  print (dbn,sizes)
  let lastLayerSize :: Int
      lastLayerSize = last sizes
      
      randomNeuron :: IO ([[[Double]]], [[Double]])
      randomNeuron = do
        (([[weights'best]],_bias),_score) <- readIORef bestNeuronRef
        
        let actualRange = (fromIntegral thrnum) * localSearchRange
 
        weights' <- (replicateM lastLayerSize (uniformR (1-actualRange,1+actualRange) gen))
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
                             putStrLn (printf "[%d] LOCAL NEURON %s" thrnum (show neuron))
                             let ccount = length constraints
                             putStrLn (printf "[%d] LOCAL SCORE %f (cnum=%d, bad=%d)" thrnum score ccount (ceiling $ fromIntegral ccount * (1-score) :: Int) )
            newBest (neuron,score,action)
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,neginf)

-- helpers 

neginf :: Double
neginf = negate (1/0)

scoreNeuron :: (Repr (GameRepr g), Game2 g) => SingleNeuron -> TNetwork -> [Constraint g] -> Double
scoreNeuron n dbn constraints = scoreConstraints (evalNeuronGame n dbn) constraints

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
