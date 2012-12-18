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


type LastLayer = (Double,[Double])

data AgentNN = AgentNN { 
                         net :: Network Double
                       , lastLayer :: LastLayer
                       , col :: Color
                       } deriving (Eq, Show, Ord)



instance Agent AgentNN where
    mkAgent col = do
      (neuralNetwork, sizes) <- parseNetFromFile'
#if TRYRANDOMNETWORK
#if NEWHNN
      -- check how random network is doing
      neuralNetwork <- AI.HNN.FF.Network.createNetwork (3*61) [100]
#endif
#endif

      let ll = (0, replicate (last sizes) 1)
      return (AgentNN neuralNetwork ll col)
    makeMove agent brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNet (gtColorNow g) (gtBoard g) (net agent) (lastLayer agent)) 
                              (col agent) (col agent)
          depth = 1
          (princ, score) = GTreeAlgo.negascout gst depth
      -- when (score /= 0) (print ("agent-nn",score,lastLayer agent,col agent))
      return (gtBoard $ head $ tail $ princ)

doubleToEvalInt :: Double -> Int
doubleToEvalInt d = round (d * 1000000)

parseNetFromFile' = parseNetFromFile `fmap` readFile "nn.txt"

parseNetFromFile :: String -> (Network Double, [Int])
parseNetFromFile input = asserts $ (result, sizes) -- (length weights, length weights'split, length biases, neuronCount, layerCount, sizes)
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

evalBoardNet :: Color -> Board -> Network Double -> LastLayer -> Double
evalBoardNet col brd net (ll'b, ll'w) = result
    where
      brdEval = if col == White then brd else negateBoard brd

      i2d 1 = 1.0
      i2d 0 = 0.0
      i2d _ = error "This function is not defined for values other than 1 and 0."

      values = map i2d $ boardToSparse brdEval

      net'll :: Network Double
      net'll = loadNetwork [[ll'b]] [[ll'w]]
      combine = U.sum -- assumed to work on single value here
      result'p1 = computeNetworkWith net sigmoid (U.fromList values)
      result'p2 = computeNetworkWith (net'll) id result'p1
      result = combine result'p1
 
-- #if NEWHNN
--       combine = U.sum -- TODO: better function!
--       result = combine (computeNetworkWith net sigmoid (U.fromList values))
-- #else
--       combine = sum
--       result = combine (computeNet'long net values)
-- #endif

      

g0 :: (Num a) => [a]
g0 = [1,0,1,0,0,0,0,0,1,1,1,1,0,1,1,0,1,1,1,0,1,1,1,0,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,0,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0]

