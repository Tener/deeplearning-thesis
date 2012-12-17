{-# LANGUAGE CPP, ViewPatterns #-}

module NeuralNets where

#if NEWHNN
import AI.HNN.FF.Network -- from hnn-0.2 package
import qualified Data.Vector.Unboxed as U
#else
import AI.HNN.Neuron -- from hnn-0.1 package
import AI.HNN.Net
import AI.HNN.Layer
#endif


import Control.Monad(when)
import qualified Data.Tree.Game_tree.Negascout as GTreeAlgo
import Text.Printf
import Data.List.Split(splitPlaces)

import Board
import CommonDatatypes

#if !NEWHNN
type Network a = [[Neuron]]
#endif


data AgentNN = AgentNN { 
                         net :: Network Double
                       , lastLayer :: [(Double,[Double])]
                       , col :: Color
                       }



instance Agent AgentNN where
    mkAgent col = do
      neuralNetwork <- parseNetFromFile'
      return (AgentNN neuralNetwork [] col)
    makeMove agent brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNet (gtColorNow g) (gtBoard g) (net agent)) (col agent) (col agent)
          depth = 1
          (princ, score) = GTreeAlgo.negascout gst depth
      -- when (True) (print ("agent-nn",score,col agent))
      return (gtBoard $ head $ tail $ princ)

doubleToEvalInt :: Double -> Int
doubleToEvalInt d = round (d * 10000)

parseNetFromFile' = parseNetFromFile `fmap` readFile "/home/tener/nn.txt"

parseNetFromFile :: String -> Network Double
parseNetFromFile input = asserts $ result -- (length weights, length weights'split, length biases, neuronCount, layerCount, sizes)
  where input'lines@(sizes'row : rest'rows) = lines input
        sizes = readIntsRow sizes'row -- sizes in first line

        neuronCount = sum sizes
        layerCount = length sizes

        rest'double'rows :: [[Double]]
        rest'double'rows = map readDoublesRow rest'rows

        weights, biases :: [[Double]]
        (biases,moar) = splitAt layerCount rest'double'rows -- biases in next layerCount lines -- in each appropriate number of biases
        (weights,garbage) = splitAt neuronCount $ moar -- weights in next neuronCount lines
        weights'split = splitPlaces sizes weights

        --- weights, biases, neuronCount, layerCount, sizes
        biases'neg = (map (map negate) biases)  -- must negate biases -- different from matlab

#if NEWHNN
        result = network
        network = loadNetwork biases'neg weights'split
#else
        result = neurons
        neurons = zipWith makeOneLayer biases'neg weights'split
        -- helper
        makeOneLayer :: [Double] -> [[Double]] -> [Neuron]
        makeOneLayer biases weights = zipWith createNeuronSigmoid biases weights
#endif

        asserts r | garbage /= [] = error (printf "parseNetFromFile: garbage not empty: %d elements" (length garbage))
                  | length weights /= neuronCount = error (printf "parseNetFromFile: too little weights: %d (should be %d)" (length weights) neuronCount)
                  | length weights'split /= layerCount = error "parseNetFromFile: not enough weights?"
                  | otherwise = r -- r for result


readDoublesRow :: String -> [Double]
readDoublesRow row = map read (words row)

readIntsRow :: String -> [Int]
readIntsRow row = map read (words row)

evalBoardNet :: Color -> Board -> Network Double -> Double
evalBoardNet col brd net = result
    where
      brdEval = if col == White then brd else negateBoard brd

      i2d 1 = 1.0
      i2d 0 = 0.0
      i2d _ = error "This function is not defined for values other than 1 and 0."

      values = map i2d $ boardToSparse brdEval
 

#if NEWHNN
      combine = U.sum -- TODO: better function!
      result = combine (computeNetworkWith net sigmoid (U.fromList values))
#else
      combine = sum
      result = combine (computeNet'long net values)
#endif

g0 :: (Num a) => [a]
g0 = [1,0,1,0,0,0,0,0,1,1,1,1,0,1,1,0,1,1,1,0,1,1,1,0,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,0,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0]

