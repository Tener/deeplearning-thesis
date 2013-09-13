{-# LANGUAGE OverloadedStrings, FlexibleContexts, BangPatterns, ImplicitParams, Rank2Types, TypeFamilies, CPP #-} 
{-# LANGUAGE ScopedTypeVariables #-}
-- | various utility functions for writing near-complete experiments with generic games (@Game2@)

module GenericGameExperiments where

import AgentGeneric
-- import Board
import BreakthroughGame
import ConstraintsGeneric
import GenericGame
import LittleGolem
import Matlab
import MinimalNN
-- import GraphNN
import ThreadLocal
import MyVectorType
import Utils (shuffle, ofType)

import Data.Default
import System.FilePath
import System.Directory
import Control.Arrow ((&&&))
import Control.Applicative ((<$>))
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
import Data.Text (Text)
import Text.Printf
import System.ProgressBar as Bar
-- import Data.Packed.Vector (Vector)

#ifndef WINDOWS
import System.Posix.Signals
#endif

import qualified Control.Concurrent.Timeout as Timeout
import Data.Timeout

-- type MyGameA = Abalone
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
genConstraints = Prelude.concat `fmap` parWorkThreads constraintCount genConstraintsCnt


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
      cnt num | num <= re = oneThr+1
              | otherwise = oneThr
  let mvar'stdout = tl_stdout ?thrLoc
  ccs <- mapConcurrently (\ thr -> runThrLocIO (ThreadLocal mvar'stdout (show thr)) (fun (cnt thr)))
                         [1..threads] 
  return ccs

evaluateWinnersCountDef :: Int
evaluateWinnersCountDef = 50

interruptible :: ThrLocIO () -> ThrLocIO ()
interruptible act = do
  a1 <- async act
  a2 <- async $ do
          handlerVar <- newEmptyMVar 
          installUser1 (putMVar handlerVar ())
          takeMVar handlerVar
          printTL ("USR1 received, interrupting." :: String)
  _ <- waitAnyCancel [a1,a2]
  return ()

reportWin :: (Agent2 a1, Agent2 a2) => a1 -> a2 -> Player2 -> ThrLocIO Double
reportWin ag1 ag2 pl = reportWinCount evaluateWinnersCountDef ag1 ag2 pl

reportWinCount :: (Agent2 a1, Agent2 a2) => Int -> a1 -> a2 -> Player2 -> ThrLocIO Double
reportWinCount evaluateWinnersCount ag1 ag2 pl = do
              winRef <- newIORef (0,0,0)

              let calculateWinnersPCT g d = do
                    let w = if (winner (g :: MyGame) == Just pl) then 1 else 0
                    (wins, _totalDepth, played) <- atomicModifyIORef winRef (\ (!cnt, !dep, !played) -> ((id &&& id) (cnt+w, dep+d, played+1)))
                    let pct = 100 * (fromIntegral wins) / (fromIntegral (played :: Int)) :: Double
                        progress = 100 * (fromIntegral played) / (fromIntegral evaluateWinnersCount) :: Double
                        latest = case winner g of
                                   Nothing -> "UNDECIDED" :: String
                                   Just plW -> if plW == pl then "WIN" else "LOSE"
                    putStrLnTL (printf "WINS: %d/%d (%0.2f%%), latest: %s, progress %0.2f%%" wins played pct latest progress)
                    
              interruptible $ void $ parWorkThreads evaluateWinnersCount (\ cnt -> sampleGameDepthCount ag1 ag2 cnt calculateWinnersPCT)

              (winCount,depths,played) <- readIORef winRef
              let winPCT = 100 * ((fromIntegral winCount) / (fromIntegral played)) :: Double
                  depthAVG = (fromIntegral depths) / (fromIntegral played) :: Double
                  n1 = if pl == P1 then agentName ag1 else agentName ag2
                  n2 = if pl == P2 then agentName ag1 else agentName ag2
              putStrLnTL (printf "%s[%s] won vs [%s] in %d matches, win percentage: %0.2f%%, avg depth=%0.2f" (show pl) n1 n2 (winCount :: Int) winPCT depthAVG :: String)

              return winPCT

getRandomFileName :: IO String
getRandomFileName = (map toEnum) `fmap` replicateM 20 (withSystemRandom $ asGenIO $ uniformR (fromEnum 'a',fromEnum 'z'))

-- | compress file with gzip removing original file.
compressRemoveFile :: FilePath -> IO FilePath
compressRemoveFile file'orig = do
  let file'out = file'orig <.> "gzip"
  BSL.writeFile file'out =<< GZip.compress `fmap` BSL.readFile file'orig
  removeFile file'orig
  return file'out

-- train DBN on randomly sampled @sampleCount@ games of type @game@. Returns filepath with DBN.
-- sampleGamesTrainNetwork :: (Repr (GameRepr g), Game2 g) => g -> Int -> Float -> Maybe MatlabOpts -> ThrLocIO FilePath
-- sampleGamesTrainNetwork game sampleCount prob mlopts = do
--   outputDir <- ("tmp-data" </>) `fmap` getRandomFileName
--   createDirectoryIfMissing True outputDir
--   filename'data <- (\f -> outputDir </> f <.> "csv") `fmap` getRandomFileName
--  
--   withFile filename'data WriteMode $ \ han -> do
--       sR <- newIORef sampleCount
--       let cb g = do
--             BSC8.hPutStrLn han (serializeGame (ofType g game))
--             atomicModifyIORef sR (\ !d -> (d-1,()))
--       sampleRandomGames ((>0) `fmap` readIORef sR) prob cb
--       hFlush han
--  
-- --  filename'data'comp <- compressRemoveFile filename'data
--   print =<< prepAndRun (fromMaybe def mlopts) outputDir filename'data
--   return (outputDir </> "dbn.txt")

-- | train DBN on randomly sampled @sampleCount@ games of type @game@. Returns filepath with DBN.
sampleGamesTrainNetwork :: (Repr (GameRepr g), Game2 g) => g -> Int -> Float -> Maybe MatlabOpts -> ThrLocIO FilePath
sampleGamesTrainNetwork game sampleCount prob mlopts = do
  outputDir <- ("tmp-data" </>) `fmap` getRandomFileName
  createDirectoryIfMissing True outputDir
  filename'data <- (\f -> outputDir </> f <.> "csv") `fmap` getRandomFileName

  withFile filename'data WriteMode $ \ hanRaw -> do
    hanVar <- newMVar hanRaw
    sR <- newIORef sampleCount
    let cb g = withMVar hanVar $ \ hanShared -> do
          BSC8.hPutStrLn hanShared (serializeGame (ofType g game))
          remaining <- atomicModifyIORef sR (\ !d -> (d-1,d-1))
          let progress = total - fromIntegral remaining
              total = fromIntegral sampleCount
          when (progress `mod` 1000 == 0) (progressBar (Bar.msg "sampleGamesTrainNetwork") Bar.exact 120 progress total)
    void $ parWorkThreads sampleCount (\ cnt -> sampleRandomGamesCount cnt prob cb)
  print =<< prepAndRun (fromMaybe def mlopts) outputDir filename'data
  return (outputDir </> "dbn.txt")

-- | train DBN on randomly sampled @sampleCount@ games of type @game@. Returns filepath with DBN.
--   also store expected answers calculated by MCTS.
sampleGamesTrainNetworkSolveMCTS :: (Repr (GameRepr g), Game2 g) => g -> Int -> Float -> Maybe MatlabOpts -> Int -> ThrLocIO FilePath
sampleGamesTrainNetworkSolveMCTS game sampleCount prob mlopts mctsLevel = do
  outputDir <- ("tmp-data" </>) `fmap` getRandomFileName
  createDirectoryIfMissing True outputDir
  data'rnd'name <- getRandomFileName
  let filename'data = outputDir </> (data'rnd'name ++ "_data") <.> "csv"
      filename'train = outputDir </> (data'rnd'name ++ tag) <.> "csv"
      tag = printf "_training_MCTS_%d" mctsLevel

  sR <- newIORef sampleCount
  let tickProgressCounter = do
        remaining <- atomicModifyIORef sR (\ !d -> (d-1,d-1))
        let progress = total - fromIntegral remaining
            total = fromIntegral sampleCount
        when (progress `mod` 500 == 0) (do
                                           putStrLn ""
                                           putStrLnTL "-- ping --"
                                           putStrLn ""
                                       )
        progressBar (Bar.msg "sampleGamesTrainNetworkSolveMCTS") Bar.exact 120 progress total

  withFile filename'data WriteMode $ \ hanRaw -> do
    withFile filename'train WriteMode $ \ hanTrain -> do
      hanVar <- newMVar hanRaw
      let cb g = do
            agMCTS <- mkAgent mctsLevel
            !evaluation <- evaluateGameM (agMCTS :: AgentMCTS) P1 g
            withMVar hanVar $ \ hanShared -> do
              BSC8.hPutStrLn hanShared (serializeGame (ofType g game))
              BSC8.hPutStrLn hanTrain (serializeRepr [evaluation])
              tickProgressCounter
      void $ parWorkThreads sampleCount (\ cnt -> sampleRandomGamesCount cnt prob cb)

  -- fixme!!! add nn training here
  print =<< prepAndRun (fromMaybe def mlopts) outputDir filename'data
  let dbnFile = (outputDir </> "dbn.txt")
  return dbnFile


-- | all recorded games 
allGameRecords = map ("data-good" </>) ["player_game_list_breakthrough_DavidScott.txt" , "player_game_list_breakthrough_edbonnet.txt", "player_game_list_breakthrough_halladba.txt", "player_game_list_breakthrough_kyledouglas.txt", "player_game_list_breakthrough_MojmirHanes.txt", "player_game_list_breakthrough_MojoRising.txt", "player_game_list_breakthrough_RayGarrison.txt", "player_game_list_breakthrough_StopSign.txt", "player_game_list_breakthrough_vic.txt", "player_game_list_breakthrough_wanderer_c.txt"]  

-- | train DBN on randomly mutated games derived from real-life data.
mutateRealGamesTrainNetwork :: (LittleGolemMoveParser g, Repr (GameRepr g), Game2 g) => g -> [FilePath] -> Int -> Float -> Double -> Maybe MatlabOpts -> ThrLocIO FilePath
mutateRealGamesTrainNetwork game0 sourceFiles sampleCount' gameDepthProb pctRnd mlopts = do
  let sampleCount'1, sampleCount'2 :: Int
      sampleCount'1 = sampleCount' - sampleCount'2
      sampleCount'2 = round ((fromIntegral sampleCount') * (pctRnd :: Double))
      -- pctRnd = 0.5
  outputDir <- ("tmp-data" </>) `fmap` getRandomFileName
  createDirectoryIfMissing True outputDir
  filename'data <- (\f -> outputDir </> f <.> "csv") `fmap` getRandomFileName

  mygen <- withSystemRandom $ asGenIO $ return  
  agRnd <- mkAgent () :: ThrLocIO AgentRandom

  records <- mapM parseGameFileName sourceFiles
  let recordsFlat = concatMap moveSequence $ filter (\ r -> result r `elem` ["1-0","0-1"]) $ Prelude.concat records
  printTL (length $ recordsFlat)
  gameStates <- cycle <$> shuffle recordsFlat -- must be infinite and non-empty
  
  withFile filename'data WriteMode $ \ han -> do

    let go games n | n > 0 = do
           let cb = GameDriverCallback (\ _ -> return ()) (\ _ _ -> do
                                                              chance <- uniform mygen
                                                              return (chance > gameDepthProb))
           newGame <- driverG2 ((head games) `ofType` game0) agRnd agRnd cb
           if winner newGame /= Nothing 
            then go (tail games) n -- game finished, ignore that state
            else BSC8.hPutStrLn han (serializeGame newGame) >> go (tail games) (n-1)
                   | otherwise = return ()
    
    go gameStates sampleCount'1 

--    forM_ (zip [1..sampleCount'1] gameStates) $ \ (cnt,game) -> do
--      let cb = GameDriverCallback (\ _ -> return ()) (\ _ _ -> do
--                                                         chance <- uniform mygen
--                                                         return (chance > gameDepthProb))
--      newGame <- driverG2 (game `ofType` game0) agRnd agRnd cb
--      BSC8.hPutStrLn han (serializeGame newGame)

    sR <- newIORef sampleCount'2
    let cb g = do
          BSC8.hPutStrLn han (serializeGame (ofType g game0))
          atomicModifyIORef sR (\ !d -> (d-1,()))
    sampleRandomGames ((>0) `fmap` readIORef sR) gameDepthProb cb
    
  print =<< prepAndRun (fromMaybe def mlopts) outputDir filename'data
  return (outputDir </> "dbn.txt")

getDBNCachedOrNew :: Bool -> Int -> Float -> Maybe MatlabOpts -> ThrLocIO FilePath
getDBNCachedOrNew useCachedDBN gameCount gameProb matlabOpts = do
  let fnAb = return "tmp-data/mlubiwjdnaaovrlgsqxu/dbn.txt"
      -- fnBr = return "tmp-data/irlfjflptuwgzpqzejrd/dbn.txt"
      -- fnBr = return "tmp-data/esodbghkmfiofntjxlph/dbn.txt" -- 1000, 1000
      fnBr = return "tmp-data/cixczsjvjhcawrnsjtpv/dbn.txt" -- (8,8), 750
      fnTN = sampleGamesTrainNetwork (freshGameDefaultParams :: MyGame) gameCount gameProb matlabOpts

      isAbalone = False -- (serializeRepr $ toRepr someGame) == (serializeRepr $ toRepr (freshGameDefaultParams :: Abalone))
      isBreakthrough = (serializeRepr $ toRepr someGame) == (serializeRepr $ toRepr (freshGameDefaultParams :: Breakthrough))

  fn <- case (isAbalone, isBreakthrough, useCachedDBN) of
          (True, False, True) -> fnAb
          (False, True, True) -> fnBr
          (_, _, _) -> fnTN

  return fn

getConstraintsPlayer :: Int -> FilePath -> Text -> ThrLocIO [(MyGame, MyGame)]
getConstraintsPlayer playerUseCoinstraints fp playerName = do
  records <- parseGameFileName fp
  let rec0 = filter ((playerName==) . playerWhite) records
      rec1 = filter (("1-0"==) . result) rec0
      rec2 = map moveSequence rec1
      constraints = concatMap (generateConstraintsGameplay P1) rec2
  printTL ("Total constraints pairs read"::String,length constraints)
  return (take playerUseCoinstraints constraints)

gameplayConstraints, gameplayConstraints'0, gameplayConstraints'1 :: (FilePath, Text)

gameplayConstraints'0 = ("data-good/player_game_list_breakthrough_RayGarrison.txt", "Ray Garrison")
gameplayConstraints'1 = ("data-good/player_game_list_breakthrough_DavidScott.txt", "David Scott")
gameplayConstraints = gameplayConstraints'0

data ConstraintSource = CS_Cache | CS_Generate | CS_Gameplay Int (FilePath, Text) deriving Show

getConstraints :: ConstraintSource -> ThrLocIO [(MyGame, MyGame)]
getConstraints constraintSource = shuffle =<< case constraintSource of
                   CS_Gameplay playerUseCoinstraints (filepath,playerName) -> getConstraintsPlayer playerUseCoinstraints filepath playerName 
                   CS_Generate -> genConstraints
                   CS_Cache    -> genConstraintsCached

installUser1 :: IO () -> IO ()
#ifndef WINDOWS
installUser1 act = installHandler sigUSR1 (CatchOnce act) Nothing >> return ()
#else
installUser1 _ = return ()
#endif

installUser2 :: IO () -> IO ()
#ifndef WINDOWS
installUser2 act = installHandler sigUSR2 (CatchOnce act) Nothing >> return ()
#else
installUser2 _ = return ()
#endif

foreverUntilFileChanged :: FilePath -> ThrLocIO () -> ThrLocIO ()
foreverUntilFileChanged filename action = do
  let timestamp = getModificationTime filename -- "src/gg-exp3.hs"
  ts0 <- timestamp
  while ((==ts0) `fmap` timestamp) action
  printTL ("Source file changed, exiting" :: String)

packConstraint :: (Functor f, Game2 a, NeuralNetwork nn) =>
                  nn -> f a -> f (Vector Double)
packConstraint dbn cons = fmap (packGame dbn) cons

packGame :: (NeuralNetwork nn, Game2 a) =>
            nn -> a -> Vector Double
packGame dbn game = computeNetworkSigmoid dbn $ toReprNN game

withTimeout :: IORef (a, Double) -> Timeout -> ThrLocIO [Async (a, Double)] -> ThrLocIO [Async (a, Double)]
withTimeout bestRef searchTimeout act = do
              asyncs <- act 
              let loop = do
                    best'0 <- snd `fmap` readIORef bestRef
                    Timeout.threadDelay searchTimeout
                    best'1 <- snd `fmap` readIORef bestRef
                    case ((best'1 == neginf),(best'0 == best'1)) of
                      (True,_) -> printTL ("Still waiting for first improvement (score=neginf)..." :: String, searchTimeout) >> loop
                      (False, True) -> printTL ("Unable to improve, timeout" :: String, searchTimeout) >> readIORef bestRef 
                      (False, False) -> printTL ("Improvement found, timeout postponed" :: String, searchTimeout) >> loop
              handlerVar <- newEmptyMVar
              let handler = do
                                installUser1 (readIORef bestRef >>= putMVar handlerVar)
                                val <- takeMVar handlerVar
                                printTL ("USR1 received, interrupting." :: String)
                                return val
              sigHandlerAsync <- async handler
              timeoutAsync <- async loop
              return (sigHandlerAsync:timeoutAsync:asyncs)


searchCB :: (Ord a, Show t, Show a) =>
            IORef (t, a) -> (t, a, IO ()) -> ThrLocIO Bool
searchCB ref = (\ (!bnNew,!bsNew,!acNew) -> do
                        let newTrim = (bnNew, bsNew)
                        updated <- atomicModifyIORef 
                                    ref (\ old@(_bnOld, !bsOld) -> do
                                                       if bsOld < bsNew
                                                        then (newTrim, True)
                                                        else (old,False))
                        when updated (printTL newTrim >> acNew)
                        return True
                     )



evaluateLL :: (NeuralNetwork nn, Show b) => nn -> b -> ThrLocIO [Double]
evaluateLL evalNetwork score = do
    putStrLnTL $ printf "FINAL SCORE %s" (show $ score)

    printTL ("BEGIN EVALUATE" :: String)

--    let llNetwork = uncurry mkTNetwork (fst bestFinal)
--        evalNetwork = appendNetwork dbn llNetwork
    
    agSmpl :: AgentTrace (AgentSimple nn) <- mkTimed "simple" evalNetwork
    -- agTree <- mkTimed "tree" (evalNetwork, 3) :: IO (AgentTrace AgentGameTree)
    -- agMtcNet <- mkTimed "mtcNet" (2, 5, evalNetwork) :: IO (AgentTrace (AgentParMCTS AgentSimple))    
    agRnd <- mkTimed "random" () :: IO (AgentTrace AgentRandom)
    agMTC <- mkTimed "mcts" 50 :: IO (AgentTrace AgentMCTS)

    putStrLnTL "======================================================================================"
    w1 <- reportWin agSmpl agMTC P1
    w2 <- reportWin agSmpl agRnd P1
    -- w3 <- reportWin agRnd agSmpl P2
    -- w2 <- reportWin agTree agMTC P1
    --  reportWin agMtcNet agMTC P1
    putStrLnTL "======================================================================================"

    return [w1, w2]

evaluateNetworkParams :: (NeuralNetwork nn) => nn -> Int -> Int -> ThrLocIO Double
evaluateNetworkParams evalNetwork evalCount mtcsCount = do
    printTL ("evaluateNetworkParams::start" :: String)
    agSmpl :: AgentTrace (AgentSimple nn) <- mkTimed "simple" evalNetwork
    agRnd <- mkTimed "random" () :: IO (AgentTrace AgentRandom)
    agMTC <- mkTimed "mcts" mtcsCount :: IO (AgentTrace AgentMCTS)
    putStrLnTL "======================================================================================"
    w1 <- reportWinCount evalCount agSmpl agMTC P1
    w2 <- reportWinCount (evalCount*10) agSmpl agRnd P1
    putStrLnTL "======================================================================================"
    printTL ("evaluateNetworkParams::finished" :: String)
    return (w1/100) -- [w1, w2]
