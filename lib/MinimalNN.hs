{-# LANGUAGE BangPatterns, FlexibleInstances #-} 

-- | minimal NN implementation based on hmatrix

module MinimalNN where

import Data.Packed.Vector as Vector
import Data.Packed.Matrix as Matrix 
import Numeric.Container
import Numeric.LinearAlgebra () -- instances

data TNetwork = TNetwork { weights :: [Matrix Double] 
                         , biases :: [Vector Double]
                         } deriving (Show, Ord, Eq)

instance Ord (Matrix Double) where
    compare a b = compare (toLists a) (toLists b)

computeTNetworkSigmoid :: TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoid (TNetwork ws bs) inp0 = foldl (\ inp (w,b) -> cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (zip ws bs)

mkTNetwork :: [[[Double]]] -> [[Double]] -> TNetwork
mkTNetwork w b | length w == length b = TNetwork (map (Matrix.trans . Matrix.fromLists) w) (map Vector.fromList b)
               | otherwise = error "Inequal number of layers for biases and weights"

sigmoid :: Floating a => a -> a
sigmoid !x = 1 / (1 + exp (-x))
{-# INLINE sigmoid #-}
