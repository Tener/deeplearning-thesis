{-# LANGUAGE CPP #-} 
module Main where

import Config

import Criterion
import Criterion.Main

import System.Random.MWC
import Text.Printf
import Data.List.Split(splitPlaces)

import AI.HNN.FF.Network as HNN
import qualified Data.Vector.Unboxed as U

import AI.Network as HasNN
import AI.Calculation as HasNN
import qualified Data.Packed.Vector as V

import qualified MinimalNN as TN

readDoublesRow :: String -> [Double]
readDoublesRow row = map read (words row)

readIntsRow :: String -> [Int]
readIntsRow row = map read (words row)

parseNetFromFile = do
  input <- readFile =<< fetchConfig configNeuralNetworkSparse -- "nn.txt"
  let input'lines@(sizes'row : rest'rows) = lines input
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

      network'01 = Network { activation = getActivation Sigmoid
                           , derivative = getDerivative Sigmoid
                           , lambda = 0
                           , weights = V.fromList (concat weights ++ concat biases)
                           , architecture = (183:sizes)
                           }

      network'02 = loadNetwork biases'neg weights'split

      network'03 = TN.mkTNetwork weights'split biases

  print (length weights, length weights'split, length biases, neuronCount, layerCount, sizes)
  return $! (network'01, network'02, network'03)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  (network'01, network'02, network'03) <- parseNetFromFile
  vec'01 <- withSystemRandom . asGenIO $ \gen -> uniformVector gen (3*61)
  vec'02 <- withSystemRandom . asGenIO $ \gen -> uniformVector gen (3*61)

  defaultMain [
        bench "network'01" $ whnf (networkOutput network'01) vec'01,
        bench "network'02" $ whnf (computeNetworkWith network'02 sigmoid) vec'02,
        bench "network'03" $ whnf (TN.computeTNetworkSigmoid network'03) vec'01,
        bench "fib" $ nf fib 20
       ]
