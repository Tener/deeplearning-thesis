{-# LANGUAGE BangPatterns, FlexibleInstances, DeriveDataTypeable #-} 

-- | minimal NN implementation based on hmatrix

module MinimalNN where

import Data.Packed.Vector as Vector
import Data.Packed.Matrix as Matrix 
import Numeric.Container
import Numeric.LinearAlgebra () -- instances

import Data.Typeable

data TNetwork = TNetwork { weights :: [Matrix Double] 
                         , biases :: [Vector Double]
                         } deriving (Show, Ord, Eq, Typeable)

instance Ord (Matrix Double) where
    compare a b = compare (toLists a) (toLists b)

computeTNetworkSigmoid :: TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoid (TNetwork ws bs) inp0 = foldl (\ inp (w,b) -> cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (zip ws bs)

computeTNetworkSigmoidSteps :: Int -> TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoidSteps steps (TNetwork ws bs) inp0 = foldl (\ inp (w,b) -> cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (take steps $ zip ws bs)


mkTNetwork :: [[[Double]]] -> [[Double]] -> TNetwork
mkTNetwork w b | length w == length b = TNetwork (map (Matrix.trans . Matrix.fromLists) w) (map Vector.fromList b)
               | otherwise = error "Inequal number of layers for biases and weights"

-- | return last layer in nn, assuming it is one neuron big
lastLayerTN :: TNetwork -> [(Double,[Double])]
lastLayerTN tn = zip b w
    where
      b = Vector.toList $ last $ biases $ tn
      w = Matrix.toLists $ Matrix.trans $ last $ weights $ tn

sigmoid :: Floating a => a -> a
sigmoid !x = 1 / (1 + exp (-x))
{-# INLINE sigmoid #-}
