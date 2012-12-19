{-# LANGUAGE CPP #-} 
module Main where

import Criterion
import Criterion.Main

import System.Random.MWC
import Text.Printf
import Data.List.Split(splitPlaces)

#ifdef BENCH01
#error BENCH01 not finished
#endif

#ifdef BENCH02
import AI.HNN.FF.Network
import qualified Data.Vector.Unboxed as U
#endif

#ifdef BENCH03
import AI.Network
import AI.Calculation
import qualified Data.Packed.Vector as V
#endif

readDoublesRow :: String -> [Double]
readDoublesRow row = map read (words row)

readIntsRow :: String -> [Int]
readIntsRow row = map read (words row)


-- parseNetFromFile :: String -> (Network Double, [Int])
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


#ifdef BENCH01
#error BENCH01 not finished
#endif

#ifdef BENCH02
        result = computeNetworkWith network sigmoid . U.fromList
        network = loadNetwork biases'neg weights'split
#endif

#ifdef BENCH03
        result = network
        network = Network { activation = getActivation Sigmoid
                          , derivative = getDerivative Sigmoid
                          , lambda = 0
                          , weights = V.fromList (concat weights)
                          , architecture = sizes
                          }
#endif

        asserts r | garbage /= [] = error (printf "parseNetFromFile: garbage not empty: %d elements" (length garbage))
                  | length weights /= neuronCount = error (printf "parseNetFromFile: too little weights: %d (should be %d)" (length weights) neuronCount)
                  | length weights'split /= layerCount = error "parseNetFromFile: not enough weights?"
                  | otherwise = r -- r for result


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  network <- parseNetFromFile `fmap` readFile "nn.txt"
  vec <- withSystemRandom . asGenIO $ \gen -> uniformVector gen (3*61)
  defaultMain [
        bench "network" $ nf (networkOutput (network :: Network)) vec,
        bench "fib" $ nf fib 20
       ]
