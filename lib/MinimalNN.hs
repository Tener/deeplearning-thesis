{-# LANGUAGE BangPatterns, FlexibleInstances, DeriveDataTypeable #-} 
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | minimal NN implementation

module MinimalNN where

import MyVectorType as V

import Data.Typeable
import Data.Binary
import GHC.Generics (Generic)
import Data.List (foldl')

data TNetwork = TNetwork { weights :: ![Matrix Double] 
                         , biases :: ![Vector Double]
                         } deriving (Show, Ord, Eq, Typeable, Read, Generic)

instance Binary TNetwork

class NeuralNetwork nn where
    computeNetworkSigmoid :: nn -> Vector Double -> Vector Double
    showNetDims :: nn -> String
    showNetName :: nn -> String

instance NeuralNetwork TNetwork where
    computeNetworkSigmoid = computeTNetworkSigmoid
    showNetDims nn = show $ (map V.dim (biases nn))
    showNetName _ = "TNetwork"

encodeFile :: FilePath -> TNetwork -> IO ()
encodeFile fn (TNetwork ws bs) = Data.Binary.encodeFile fn (map toLists ws, map toList bs)

decodeFile :: FilePath -> IO TNetwork
decodeFile fn = do
  (ws,bs) <- Data.Binary.decodeFile fn
  return (TNetwork (map fromLists ws) (map fromList bs))

computeTNetworkSigmoid :: TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoid (TNetwork ws bs) inp0 = foldl' (\ inp (!w,!b) -> V.cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (zip ws bs)
{-# INLINE computeTNetworkSigmoid #-}

computeTNetworkSigmoidSteps :: Int -> TNetwork -> Vector Double -> Vector Double
computeTNetworkSigmoidSteps steps (TNetwork ws bs) inp0 = foldl' (\ inp (w,b) -> V.cmap sigmoid ((inp `vXm` w) `add` b) ) inp0 (take steps $ zip ws bs)

mkTNetwork :: [[[Double]]] -> [[Double]] -> TNetwork
mkTNetwork w b | length w == length b = TNetwork (map (V.trans . V.fromLists) w) (map V.fromList b)
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
      b = V.toList $ last $ biases $ tn
      w = V.toLists $ V.trans $ last $ weights $ tn

sigmoid :: Floating a => a -> a
sigmoid !x = 1 / (1 + exp (-x))
{-# INLINE sigmoid #-}
