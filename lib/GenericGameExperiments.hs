{-# LANGUAGE OverloadedStrings, FlexibleContexts, BangPatterns #-} 
-- | various utility functions for writing near-complete experiments with generic games (@Game2@)

module GenericGameExperiments where

import AgentGeneric
import GenericGame
import Matlab

import Data.Default
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.GZip as GZip
import Data.IORef
import Control.Monad
import System.Random.MWC
import System.IO
import Data.Maybe

getRandomFileName :: IO String
getRandomFileName = (map toEnum) `fmap` replicateM 20 (withSystemRandom $ asGenIO $ uniformR (fromEnum 'a',fromEnum 'z'))

-- | compress file with gzip removing original file.
compressRemoveFile :: FilePath -> IO FilePath
compressRemoveFile file'orig = do
  let file'out = file'orig <.> "gzip"
  BSL.writeFile file'out =<< GZip.compress `fmap` BSL.readFile file'orig
  removeFile file'orig
  return file'out

-- | train DBN on randomly sampled @sampleCount@ games of type @game@. Returns filepath with DBN.
sampleGamesTrainNetwork :: (Repr (GameRepr g), Game2 g) => g -> Int -> Float -> Maybe MatlabOpts -> IO FilePath
sampleGamesTrainNetwork game sampleCount prob mlopts = do
  outputDir <- ("tmp-data" </>) `fmap` getRandomFileName
  createDirectoryIfMissing True outputDir
  filename'data <- (\f -> outputDir </> f <.> "csv") `fmap` getRandomFileName

  withFile filename'data WriteMode $ \ han -> do
      sR <- newIORef sampleCount
      let cb g = do
            BSC8.hPutStrLn han (serializeGame (ofType g game))
            atomicModifyIORef sR (\ !d -> (d-1,()))

          ofType :: a -> a -> a
          ofType a _ = a

      sampleRandomGames ((>0) `fmap` readIORef sR) prob cb
      hFlush han

--  filename'data'comp <- compressRemoveFile filename'data
  print =<< prepAndRun (fromMaybe def mlopts) outputDir filename'data
  return (outputDir </> "dbn.txt")
