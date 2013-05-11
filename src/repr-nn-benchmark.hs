module Main where

import Criterion
import Criterion.Main

import BreakthroughGame
import GenericGame

import qualified Data.HashMap as HashMap
import qualified Data.Packed.Vector as V
import Data.Packed.Vector ((@>))


toReprNN'1 :: Breakthrough -> V.Vector Double
toReprNN'1 g = let 
    pos = allPos (boardSize g)
    look c p = if HashMap.lookup p (board g) == c then one else zero
    repr = [ look c p | c <- [Nothing, Just P1, Just P2], p <- pos]
 in V.fromList repr
{-# NOINLINE toReprNN'1 #-}

toReprNN'2 :: Breakthrough -> V.Vector Double
toReprNN'2 g = let 
    pos = allPos (boardSize g)
    posVec'X = V.fromList (map fst pos)
    posVec'Y = V.fromList (map snd pos)

    getElementPos v = HashMap.lookup ((posVec'X @> v),(posVec'Y @> v)) (board g) 
    boolToDouble :: Bool -> Double
    boolToDouble True = one
    boolToDouble False = zero

    (sizeX, sizeY) = boardSize g
    singleVecLen :: Int
    singleVecLen = sizeX * sizeY
    vecLen = 3 * singleVecLen
    vecEl v | v < singleVecLen   = boolToDouble ((Nothing) == (getElementPos v))
            | v < 2*singleVecLen = boolToDouble ((Just P1) == (getElementPos (v-singleVecLen)))
            | otherwise          = boolToDouble ((Just P2) == (getElementPos (v-2*singleVecLen)))

 in V.buildVector vecLen vecEl
{-# NOINLINE toReprNN'2 #-}


main = do
  let game = freshGameDefaultParams
  defaultMain [
        bench "toReprNN'2" $ whnf (toReprNN'2) game,
        bench "toReprNN'1" $ whnf (toReprNN'1) game,
        bench "toReprNN" $ whnf (toReprNN) game,
        bench "reprToNN . toRepr" $ whnf (reprToNN . toRepr) game
       ]
