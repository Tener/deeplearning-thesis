{-# LANGUAGE CPP, BangPatterns, ViewPatterns #-}

module NeuralNets where

import Control.Monad(join)
import Text.Printf
import Data.List.Split(splitPlaces)

import System.IO.Memoize (ioMemo')
import System.IO

import Config
import MinimalNN

type LastLayer = (Double,[Double])

printE :: (Show a) => a -> IO ()
printE p = do
  hPutStr stderr (show p)
  hPutStr stderr "\n"

doubleToEvalInt :: Double -> Int
doubleToEvalInt d = round (d * (fromIntegral ((maxBound :: Int) `div`10)))

#ifdef ABALONE
sparse :: IO Bool
sparse = fetchConfig configUseSparseRepr

nn'filename :: IO FilePath
nn'filename = do
  sparse' <- sparse
  let if' b t f = if b then t else f
  if' sparse' (fetchConfig configNeuralNetworkSparse) (fetchConfig configNeuralNetworkDense)

parseNetFromFile' :: IO (TNetwork, [Int])
parseNetFromFile' = do
  nnfname <- nn'filename 
  join $ parseNetFromFile'' nnfname
#endif


getDBNFile :: FilePath -> IO TNetwork
getDBNFile fn = (fst . parseNetFromFile) `fmap` readFile fn

parseNetFromFile'' :: FilePath -> IO (IO (TNetwork, [Int]))
parseNetFromFile'' fp = ioMemo' (parseNetFromFile `fmap` readFile fp)

parseNetFromFile'LL :: IO (TNetwork, [Int])
parseNetFromFile'LL = join $ parseNetFromFile'' "nn_ll.txt"


parseNetFromFile :: String -> (TNetwork, [Int])
parseNetFromFile input = asserts $ (result, sizes) -- (length weights, length weights'split, length biases, neuronCount, layerCount, sizes)
  where _input'lines@(sizes'row : rest'rows) = lines input
        sizes = readIntsRow sizes'row -- sizes in first line

        neuronCount = sum sizes
        layerCount = length sizes

        rest'double'rows :: [[Double]]
        rest'double'rows = map readDoublesRow rest'rows

        weightsR, biasesR :: [[Double]]
        (biasesR,moar) = splitAt layerCount rest'double'rows -- biases in next layerCount lines -- in each appropriate number of biases
        (weightsR,garbage) = splitAt neuronCount $ moar -- weights in next neuronCount lines
        weights'split = splitPlaces sizes weightsR

        --- weights, biases, neuronCount, layerCount, sizes
        -- biases'neg = (map (map negate) biases)  -- must negate biases -- different from matlab -- not any more

        result = network
        network = mkTNetwork weights'split biasesR

        asserts r | garbage /= [] = error (printf "parseNetFromFile: garbage not empty: %d elements" (length garbage))
                  | length weightsR /= neuronCount = error (printf "parseNetFromFile: too little weights: %d (should be %d)" (length weightsR) neuronCount)
                  | length weights'split /= layerCount = error "parseNetFromFile: not enough weights?"
                  | otherwise = r -- r for result


readDoublesRow :: String -> [Double]
readDoublesRow row = map read (words row)

readIntsRow :: String -> [Int]
readIntsRow row = map read (words row)


