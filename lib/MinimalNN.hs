{-# LANGUAGE BangPatterns, FlexibleInstances, DeriveDataTypeable #-} 
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | minimal NN implementation based on hmatrix

module MinimalNN where

import Data.Packed.Vector as Vector
import Data.Packed.Matrix as Matrix 
import Numeric.Container
import Numeric.LinearAlgebra () -- instances
import Data.List (foldl')

import Data.Typeable

data TNetwork = TNetwork { weights :: ![Matrix Double] 
                         , biases :: ![Vector Double]
                         } deriving (Show, Ord, Eq, Typeable, Read)

instance Ord (Matrix Double) where
    compare a b = compare (toLists a) (toLists b)

computeTNetworkSigmoid :: TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoid (TNetwork ws bs) inp0 = foldl' (\ inp (!w,!b) -> cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (zip ws bs)
{-# INLINE computeTNetworkSigmoid #-}

computeTNetworkSigmoidSteps :: Int -> TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoidSteps steps (TNetwork ws bs) inp0 = foldl' (\ inp (w,b) -> cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (take steps $ zip ws bs)


mkTNetwork :: [[[Double]]] -> [[Double]] -> TNetwork
mkTNetwork w b | length w == length b = TNetwork (map (Matrix.trans . Matrix.fromLists) w) (map Vector.fromList b)
               | otherwise = error "Inequal number of layers for biases and weights"
{-# INLINE mkTNetwork #-}

-- | append one network after another. input/output sizes must work out (which isn't checked until later)
appendNetwork :: TNetwork -- ^ first network, will receive input first
              -> TNetwork -- ^ second network, will be fed output of first network
              -> TNetwork -- ^ resulting network
appendNetwork tnet1 tnet2 = TNetwork (weights tnet1 ++ weights tnet2) (biases tnet1 ++ biases tnet2)
{-# INLINE appendNetwork #-}

-- | return last layer in nn, assuming it is one neuron big
lastLayerTN :: TNetwork -> [(Double,[Double])]
lastLayerTN tn = zip b w
    where
      b = Vector.toList $ last $ biases $ tn
      w = Matrix.toLists $ Matrix.trans $ last $ weights $ tn

sigmoid :: Floating a => a -> a
sigmoid !x = 1 / (1 + exp (-x))
{-# INLINE sigmoid #-}
{-# SPECIALIZE sigmoid :: Double -> Double #-}
