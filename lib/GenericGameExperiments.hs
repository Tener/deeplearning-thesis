{-# LANGUAGE OverloadedStrings, FlexibleContexts, BangPatterns, ImplicitParams, Rank2Types, TypeFamilies #-} 
-- | various utility functions for writing near-complete experiments with generic games (@Game2@)

module GenericGameExperiments where

import AgentGeneric
import Board
import BreakthroughGame
import ConstraintsGeneric
import GenericGame
import Matlab
import ThreadLocal

import Data.Default
import System.FilePath
import System.Directory
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.GZip as GZip
import Data.IORef
import Control.Monad
import System.Random.MWC
import System.IO
import Data.Maybe
import Text.Printf


import Data.Chronograph

type MyGameA = Abalone
type MyGameB = Breakthrough
type MyGame = MyGameB


someGame :: MyGame
someGame = freshGameDefaultParams

constraintCount, constraintDepth :: Int
constraintCount = 500
constraintDepth = 1000

constraintsCacheFilename :: FilePath
constraintsCacheFilename = let spec :: String
                               spec = printf "constraints-d%d-c%d-%s.txt" constraintDepth constraintCount (gameName someGame)
                           in "tmp-data" </> "cache" </> spec


genConstraints :: ThrLocIO [(MyGame, MyGame)]
genConstraints = concat `fmap` parWorkThreads constraintCount genConstraintsCnt


genConstraintsCnt :: Int -> ThrLocIO [(MyGame, MyGame)]
genConstraintsCnt conCount = do
  cR <- newIORef []
  let addConstraints cs = atomicModifyIORef cR (\ !old -> ((cs:old),()))
  
  ag <- mkAgent constraintDepth :: IO AgentMCTS
  sampleRandomGamesCount conCount 0.01 (\ g -> do
                                     cs <- generateConstraintsMCTS' ag (g :: MyGame)
                                     addConstraints cs
                                     printTL =<< length `fmap` readIORef cR
                                  )
  c <- readIORef cR
  createDirectoryIfMissing True (takeDirectory constraintsCacheFilename)
  writeFile constraintsCacheFilename (show c)
  return c


genConstraintsCached :: ThrLocIO [(MyGame,MyGame)]
genConstraintsCached = do
  b <- doesFileExist constraintsCacheFilename
  if b then read `fmap` readFile constraintsCacheFilename
       else genConstraints


parWorkThreads :: Int -> (Int -> ThrLocIO a) -> ThrLocIO [a]
parWorkThreads c fun = do
  threads <- getNumCapabilities
  let oneThr = c `div` threads
      re = c - (oneThr * threads)
      cnt 1 = oneThr+re
      cnt _ = oneThr
  let mvar'stdout = tl_stdout ?thrLoc
  ccs <- mapConcurrently (\ thr -> do
                            runThrLocIO (ThreadLocal mvar'stdout (show thr)) (fun (cnt thr)))
                         [1..threads] 
  return ccs


timed :: (Show t) => t -> IO b -> ThrLocIO b
timed s a = do
  (Chronograph r t) <- chronoIO a
  printTL (s,t)
  return r

mkTimed :: (Show label, Agent2 a, AgentParams a ~ (IOAct, arg)) => label -> arg -> ThrLocIO a
mkTimed label arg = mkAgent ((IOAct (timed label)), arg)


evaluateWinnersCount :: Int
evaluateWinnersCount = 100

reportWin :: (Agent2 a1, Agent2 a2) => a1 -> a2 -> Player2 -> ThrLocIO ()
reportWin ag1 ag2 pl = do
              winRef <- newIORef (0,0)

              let calculateWinnersPCT g d = do
                    let w = if (winner (g :: MyGame) == Just pl) then 1 else 0
                     
                    wins <- atomicModifyIORef winRef (\ (!cnt, !dep) -> ((cnt+w, dep+d), (cnt+w)))
                    if (w == 1) then (printTL ("WINS" :: String,wins)) else printTL ("LOSE" :: String,d)

              _ <- parWorkThreads evaluateWinnersCount (\ cnt -> sampleGameDepthCount ag1 ag2 cnt calculateWinnersPCT)

              (winCount,depths) <- readIORef winRef
              let winPCT = 100 * ((fromIntegral winCount) / (fromIntegral evaluateWinnersCount)) :: Double
                  depthAVG = (fromIntegral depths) / (fromIntegral evaluateWinnersCount) :: Double
                  n1 = if pl == P1 then agentName ag1 else agentName ag2
                  n2 = if pl == P2 then agentName ag1 else agentName ag2
              putStrLnTL (printf "%s[%s] won vs [%s] in %d matches, win percentage: %f%%, avg depth=%f" (show pl) n1 n2 (winCount :: Int) winPCT depthAVG :: String)

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
sampleGamesTrainNetwork :: (Repr (GameRepr g), Game2 g) => g -> Int -> Float -> Maybe MatlabOpts -> ThrLocIO FilePath
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

getDBNCachedOrNew :: Bool -> Int -> Float -> Maybe MatlabOpts -> ThrLocIO FilePath
getDBNCachedOrNew useCachedDBN gameCount gameProb matlabOpts = do
  let fnAb = return "tmp-data/mlubiwjdnaaovrlgsqxu/dbn.txt"
      -- fnBr = return "tmp-data/irlfjflptuwgzpqzejrd/dbn.txt"
      -- fnBr = return "tmp-data/esodbghkmfiofntjxlph/dbn.txt" -- 1000, 1000
      fnBr = return "tmp-data/cixczsjvjhcawrnsjtpv/dbn.txt" -- (8,8), 750
      fnTN = sampleGamesTrainNetwork (freshGameDefaultParams :: MyGame) gameCount gameProb matlabOpts

      isAbalone = toRepr someGame == toRepr (freshGameDefaultParams :: Abalone)
      isBreakthrough = toRepr someGame == toRepr (freshGameDefaultParams :: Breakthrough)

  fn <- case (isAbalone, isBreakthrough, useCachedDBN) of
          (True, False, True) -> fnAb
          (False, True, True) -> fnBr
          (_, _, _) -> fnTN

  return fn