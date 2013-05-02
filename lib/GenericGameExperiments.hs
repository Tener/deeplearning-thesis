{-# LANGUAGE OverloadedStrings, FlexibleContexts #-} 
-- | various utility functions for writing near-complete experiments with generic games (@Game2@)

module GenericGameExperiments where

import AgentGeneric
import GenericGame
--import MinimalNN
import Matlab
-- import Config

import Data.Default
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BSC8
import Data.IORef
import Control.Monad
import System.Random.MWC
import System.IO

getRandomFileName :: IO String
getRandomFileName = (map toEnum) `fmap` replicateM 20 (withSystemRandom $ asGenIO $ uniformR (fromEnum 'a',fromEnum 'z'))

-- | train DBN on randomly sampled @sampleCount@ games of type @game@. Returns filepath with DBN.
sampleGamesTrainNetwork :: (Repr (GameRepr g), Game2 g) => g -> Int -> Float -> IO FilePath
sampleGamesTrainNetwork game sampleCount prob = do
  sR <- newIORef sampleCount
  outputDir <- ("tmp-data" </>) `fmap` getRandomFileName
  createDirectoryIfMissing True outputDir
  filename'data <- (\f -> outputDir </> f <.> "csv") `fmap` getRandomFileName

  withFile filename'data WriteMode $ \ han -> do
      let cb g = do
            BSC8.hPutStrLn han (serializeGame (ofType g game))
            modifyIORef sR (\d -> d-1)
            print =<< readIORef sR

          ofType :: a -> a -> a
          ofType a _ = a

      sampleRandomGames ((>0) `fmap` readIORef sR) prob cb
      hFlush han
  
  print =<< prepAndRun def outputDir filename'data
  return (outputDir </> "dbn.txt")
