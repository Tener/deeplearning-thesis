module Constraints where

import Board
-- import CommonDatatypes
import MinimalNN
import NeuralNets

-- import Control.Monad (unless, when)
-- import Text.Printf
import System.Random.MWC

-- import qualified Data.Vector.Unboxed as U
import qualified Data.Packed.Vector as Vector

-- 
-- 

newtype WBoard = WBoard Board -- board evaluated from perspective of White
newtype BBoard = BBoard Board -- dual of WBoard

data Constraint = CBetter Board Board -- first board is better than the other
                | CBetterAll Board [Board] -- first board is better then ALL the others

-- | check if constraint is held under given evaluation function
checkConstraint :: (Board -> Double) -> Constraint -> Bool
checkConstraint f con = case con of 
                          CBetter b0 b1 -> f b0 > f b1
                          CBetterAll b bs -> let aux = f b in all (\b' -> aux > f b') bs

-- | score constraints, returns percentage true
scoreConstraints :: (Board -> Double) -> [Constraint] -> Double
scoreConstraints f cons = let len = length cons
                              true = length $ filter id (map (checkConstraint f) cons)
                          in (fromIntegral true) / (fromIntegral len)

-- | simple way to generate constraints, we dont do any recursion: just check immidiate moves
generateConstraintsSimple :: Board -> Board -> [Constraint]
generateConstraintsSimple brd'base brd'good = [CBetter brd'good brd | brd <- getMoves White brd'base, brd /= brd'good]

-- | good moves

good'moves :: [(Board,Board)]
good'moves = [ (b0,b1), (b2,b3) ]
    where
      b0 = undefined
      b1 = undefined
      b2 = undefined
      b3 = undefined

-- | load DBN, apply single neuron to DBN output

-- | benchmark random single-neuron networks

singleNeuronRandomSearch :: IO ()
singleNeuronRandomSearch = do
  gen <- withSystemRandom $ asGenIO $ return
  (dbn,sizes) <- parseNetFromFile `fmap` readFile "/home/tener/abalone-nn/nn_183.txt-100"
  let lastLayerSize :: Int
      lastLayerSize = last sizes
      
      randomNeuron :: IO ([[[Double]]], [[Double]])
      randomNeuron = do
        (bias:weights) <- Vector.toList `fmap` (uniformVector gen (lastLayerSize+1))
        return ([[weights]],[[bias]])

  let inf = 1/0

      constraints = concat $ map (uncurry generateConstraintsSimple) good'moves

      evalNeuronBoard neuron brd = 
          let final = Vector.toList $ computeTNetworkSigmoid neuronTNet 
                                    $ computeTNetworkSigmoid dbn 
                                    $ boardToSparseNN brd 
              neuronTNet :: TNetwork
              neuronTNet = uncurry mkTNetwork neuron
             in
          case final of
            [x] -> x
            [] -> error "No values returned. What?!"
            _ -> error "Too many values returned. Last layer too big."
              
                                   
      scoreNeuron n = scoreConstraints (evalNeuronBoard n) constraints
    
      go (best'neuron,best'score) = do
         neuron <- randomNeuron
         let score = scoreNeuron neuron
         if score > best'score 
          then do
            print ("SCORE",score)
            print ("NEURON",neuron)
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,negate inf)





