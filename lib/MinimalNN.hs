-- | minimal NN implementation based on hmatrix

module MinimalNN where

import Data.Packed.Vector as Vector
import Data.Packed.Matrix as Matrix 

data TNetwork = TNetwork { [Matrix Double] :: weights
                         , [Vector Double] :: biases
                         }

computeTNetworkSigmoid :: TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoid (TNetwork ws bs) inp0 = foldl (\ inp (w,b) -> cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (zip ws bs)

mkTNetwork :: [[[Double]]] -> [[Double]] -> TNetwork
mkTNetwork w b = TNetwork (map Matrix.fromLists w) (map Vector.fromList b)

{-# INLINE sigmoid #-}
sigmoid :: Floating a => a -> a
sigmoid !x = 1 / (1 + exp (-x))
