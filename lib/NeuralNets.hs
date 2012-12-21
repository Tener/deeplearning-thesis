{-# LANGUAGE CPP, BangPatterns, ViewPatterns #-}

module NeuralNets where

import Control.Monad(when, join)
import qualified Data.Tree.Game_tree.Negascout as GTreeAlgo
import Text.Printf
import Data.List.Split(splitPlaces)
import qualified Numeric.Container as NC

import System.IO.Memoize (ioMemo')
import System.IO.Unsafe

import Board
import CommonDatatypes
import MinimalNN

type LastLayer = (Double,[Double])

data AgentNN = AgentNN { net :: TNetwork
                       , lastLayer :: LastLayer
                       , col :: Color
                       } deriving (Show, Ord, Eq)

data AgentNNSimple = AgentNNSimple { netS :: TNetwork
                                   , colS :: Color
                                   } deriving (Show, Ord, Eq)

data AgentNNSimpleLL = AgentNNSimpleLL { netSLL :: TNetwork
                                       , colSLL :: Color
                                       } deriving (Show, Ord, Eq)
  
instance Agent AgentNNSimple where
    mkAgent colo = do
      let (!neuralNetwork, sizes) = myUnsafeNet
      return (AgentNNSimple neuralNetwork colo)
    makeMove (AgentNNSimple neuralNetwork colo) brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNetOnePass (gtColorNow g) (gtBoard g) neuralNetwork)
                          colo colo
          depth = 1
          (princ, score) = GTreeAlgo.negascout gst depth
      print ("AgentNNSimple", score)
      return (gtBoard $ head $ tail $ princ)

instance Agent AgentNNSimpleLL where
    mkAgent colo = do
      let (!neuralNetwork, sizes) = myUnsafeNetLL
      return (AgentNNSimpleLL neuralNetwork colo)
    makeMove (AgentNNSimpleLL neuralNetwork colo) brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNetOnePass (gtColorNow g) (gtBoard g) neuralNetwork)
                          colo colo
          depth = 5
          (princ, score) = GTreeAlgo.negascout gst depth
      print ("AgentNNSimpleLL", score)
      return (gtBoard $ head $ tail $ princ)

instance Agent AgentNN where
    mkAgent colo = do
      let (!neuralNetwork, sizes) = myUnsafeNet
          [ll] = lastLayerTN $ fst myUnsafeNetLL
--      let ll = (0, replicate (last sizes) 1)

      return (AgentNN neuralNetwork ll colo)
    makeMove agent brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNet (gtColorNow g) (gtBoard g) (net agent) (lastLayer agent)) 
                              (col agent) (col agent)
          depth = 3
          (princ, score) = GTreeAlgo.negascout gst depth
      return (gtBoard $ head $ tail $ princ)

doubleToEvalInt :: Double -> Int
doubleToEvalInt d = round (d * 10000000)

parseNetFromFile'' fp = ioMemo' (parseNetFromFile `fmap` readFile fp)
parseNetFromFile' = join $ parseNetFromFile'' "nn.txt"
parseNetFromFile'LL = join $ parseNetFromFile'' "nn_ll.txt"

{-# NOINLINE myUnsafeNet #-}
myUnsafeNet = unsafePerformIO parseNetFromFile'

{-# NOINLINE myUnsafeNetLL #-}
myUnsafeNetLL = unsafePerformIO parseNetFromFile'LL

parseNetFromFile :: String -> (TNetwork, [Int])
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
        -- biases'neg = (map (map negate) biases)  -- must negate biases -- different from matlab -- not any more

        result = network
        network = mkTNetwork weights'split biases

        asserts r | garbage /= [] = error (printf "parseNetFromFile: garbage not empty: %d elements" (length garbage))
                  | length weights /= neuronCount = error (printf "parseNetFromFile: too little weights: %d (should be %d)" (length weights) neuronCount)
                  | length weights'split /= layerCount = error "parseNetFromFile: not enough weights?"
                  | otherwise = r -- r for result


readDoublesRow :: String -> [Double]
readDoublesRow row = map read (words row)

readIntsRow :: String -> [Int]
readIntsRow row = map read (words row)

evalBoardNet :: Color -> Board -> TNetwork -> LastLayer -> Double
evalBoardNet col brd net (ll'b, ll'w) = result
    where
      brdEval = if col == White then brd else negateBoard brd

      i2d 1 = 1.0
      i2d 0 = 0.0
      i2d _ = error "This function is not defined for values other than 1 and 0."

      values = boardToSparseNN brdEval

      result'p1 = computeTNetworkSigmoid net values
      combine = NC.sumElements

      net'll :: TNetwork
      net'll = mkTNetwork [[ll'w]] [[ll'b]]
      result'p2 = computeTNetworkSigmoid net'll result'p1

      result = combine result'p2 -- fixme use 'p2

evalBoardNetOnePass :: Color -> Board -> TNetwork -> Double
evalBoardNetOnePass col brd net = result
    where
      brdEval = if col == White then brd else negateBoard brd
      values = boardToSparseNN brdEval
      result = NC.sumElements $ computeTNetworkSigmoid net values
     
g0 :: (Num a) => [a]
g0 = [1,0,1,0,0,0,0,0,1,1,1,1,0,1,1,0,1,1,1,0,1,1,1,0,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,0,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0]

