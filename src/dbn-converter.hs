{-# LANGUAGE BangPatterns, FlexibleInstances, DeriveDataTypeable #-} 
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Binary
import qualified MinimalNN
import System.Environment
import System.FilePath
import System.Directory
import Text.Printf

import GHC.Generics (Generic)
import Data.Typeable

import qualified Data.Packed.Vector as HM
import qualified Data.Packed.Matrix as HM
import qualified Numeric.Container as HM
import "hmatrix" Numeric.LinearAlgebra () 

import MyVectorType as V

data TNetwork = TNetwork { weights :: ![HM.Matrix Double] 
                         , biases :: ![HM.Vector Double]
                         } deriving (Show, Eq, Typeable, Read, Generic)
-- instance Binary TNetworkHM

fromHMNetwork :: Main.TNetwork -> MinimalNN.TNetwork
fromHMNetwork (TNetwork ws bs) = (MinimalNN.TNetwork (map (V.transFix . V.fromLists . HM.toLists) ws)
                                                     (map (V.fromList . HM.toList) bs))


main :: IO ()
main = mapM_ convertDBN =<< getArgs

convertDBN :: FilePath -> IO ()
convertDBN fn = do
  let fnOut = dropExtension fn <.> "bin"
  ex <- doesFileExist fnOut
  case ex of
    False -> do
      putStrLn $ printf "Converting: %s -> %s" fn fnOut
      net <- read `fmap` readFile fn
      MinimalNN.encodeFile fnOut (fromHMNetwork net)
    True -> do
      putStrLn $ printf "Converting: %s -> %s [skipped]" fn fnOut
