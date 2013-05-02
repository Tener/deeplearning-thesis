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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 ()
import Data.IORef
import Control.Monad
import System.Random.MWC
-- import System.Process

getRandomFileName :: IO String
getRandomFileName = do
  rgen <- (withSystemRandom $ asGenIO $ return)
  (map toEnum) `fmap` replicateM 20 (uniformR (fromEnum 'a',fromEnum 'z') rgen)

sampleGamesTrainNetwork :: (Repr (GameRepr g), Game2 g) => g -> Int -> IO ()
sampleGamesTrainNetwork game sampleCount = do
  sR <- newIORef sampleCount
  cR <- newIORef []

  let cb g = do
        modifyIORef cR ((ofType g game):)
        modifyIORef sR (\d -> d-1)
        print =<< readIORef sR

      ofType :: a -> a -> a
      ofType a _ = a

  sampleRandomGames ((>0) `fmap` readIORef sR) 0.001 cb

  games'random'sample <- readIORef cR

  outputDir <- return "tmp-data"
  filename'data <- (\f -> outputDir </> f <.> "csv") `fmap` getRandomFileName

  BS.writeFile filename'data (BS.intercalate "\n" (map serializeGame games'random'sample))

--  system "sleep 5s"

  print =<< prepAndRun def outputDir filename'data

  return ()