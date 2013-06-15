{-# LANGUAGE BangPatterns, FlexibleInstances, DeriveDataTypeable #-} 
{-# LANGUAGE DeriveGeneric #-}
-- neural networks with arbitrary backreferences, i.e. ability to insert verbatim output from previous layer
module GraphNN where

import MinimalNN hiding (weights, biases)
import MyVectorType as V

import Data.Typeable
import Data.Binary
import GHC.Generics (Generic)
import Data.List (foldl')

data GNetwork = GNetwork { weights :: ![Matrix Double] 
                         , biases :: ![Vector Double]
                         -- a matching list of backreferences.
                         , backrefs :: ![[Int]] 
                         } deriving (Show, Ord, Eq, Typeable, Read, Generic)

instance Binary GNetwork

instance NeuralNetwork GNetwork where
    computeNetworkSigmoid = computeGNetworkSigmoid
    showNetDims nn = show $ (map V.dim (biases nn))
    showNetName _ = "GNetwork"

computeGNetworkSigmoid :: GNetwork -> Vector Double -> Vector Double
computeGNetworkSigmoid (GNetwork ws bs refs) inp0 = snd $!
    foldl' (\ (prev,inp) (!w,!b,!r) -> let resp = V.cmap sigmoid ((inp `vXm` w) `add` b)
                                           whole = V.concat (resp:(map (prev!!) r)) -- here we insert based on backrefs.
                                           hist = whole : prev
                                       in (hist, whole)
           ) 
               ([],inp0) 
               (zip3 ws bs refs)
{-# INLINE computeGNetworkSigmoid #-}

-- | convert TNetwork to GNetwork
fromTNetwork :: TNetwork -> GNetwork
fromTNetwork (TNetwork ws bs) = (GNetwork ws bs (map (const []) bs))

mkGNetwork :: [[[Double]]] -> [[Double]] -> [[Int]] -> GNetwork
mkGNetwork w b r | (length w == length b) && (length b == length r) = GNetwork (map (V.trans . V.fromLists) w) (map V.fromList b) r
                 | otherwise = error "Inequal number of layers for biases and weights"
{-# INLINE mkGNetwork #-}

-- | append one network after another. input/output sizes must work out (which isn't checked until later)
appendGNetwork :: GNetwork -- ^ first network, will receive input first
               -> GNetwork -- ^ second network, will be fed output of first network
               -> GNetwork -- ^ resulting network
appendGNetwork tnet1 tnet2 = GNetwork (weights tnet1 ++ weights tnet2) 
                                      (biases tnet1 ++ biases tnet2)
                                      (backrefs tnet1 ++ backrefs tnet2)
{-# INLINE appendGNetwork #-}
