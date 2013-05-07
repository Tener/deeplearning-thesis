{-# LANGUAGE ImplicitParams, Rank2Types, BangPatterns #-}

module Main where

import Prelude hiding (putStr, putStrLn)

import BreakthroughGame
import Board

import GenericGameExperiments
import GenericGame
import AgentGeneric
import ConstraintsGeneric
import MinimalNN
import Matlab
import NeuralNets (parseNetFromFile)
import ThreadLocal

import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Text.Printf
import System.IO
import System.Directory
import System.FilePath
import Data.Default

import qualified Control.Concurrent.Timeout as Timeout
import Data.Timeout
-- import Data.Chronograph


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


main :: IO ()
main = runThrLocMainIO $ do
  printTL "Constraint generation"
  let useConstraintCache = True
  constraints <- if useConstraintCache then genConstraintsCached else genConstraints 

  let fnAb = return "tmp-data/mlubiwjdnaaovrlgsqxu/dbn.txt"
      fnBr = return "tmp-data/irlfjflptuwgzpqzejrd/dbn.txt"
      -- fnBr = return "tmp-data/esodbghkmfiofntjxlph/dbn.txt" -- 1000, 1000
      fnTN = sampleGamesTrainNetwork (freshGameDefaultParams :: MyGame) 100000 0.3 (Just (def {dbnSizes = [750]}))

      isAbalone = toRepr someGame == toRepr (freshGameDefaultParams :: Abalone)
      isBreakthrough = toRepr someGame == toRepr (freshGameDefaultParams :: Breakthrough)

      useCachedDBN = False

  printTL "DBN read/train"
  fn <- case (isAbalone, isBreakthrough, useCachedDBN) of
          (True, False, True) -> fnAb
          (False, True, True) -> fnBr
          (_, _, _) -> fnTN

  printTL ("DBN FN=",fn)

  let searchCB ref = (\ (bnNew,bsNew,acNew) -> do
                        let newTrim = (bnNew, bsNew)
                        updated <- atomicModifyIORef 
                                    ref (\ old@(_bnOld, !bsOld) -> do
                                                       if bsOld < bsNew
                                                        then (newTrim, True)
                                                        else (old,False))
                        when updated (printTL newTrim >> acNew)
                     )

  printTL "forever train last layer network & evaluate"
  let timestamp = getModificationTime "src/gg-exp3.hs"
  ts0 <- timestamp
  while ((==ts0) `fmap` timestamp) $ do
    threads <- getNumCapabilities
    bestRef <- newIORef (undefined, neginf)
    let thrG = 1
        thrL = 1
        localSearch = 0.18

        withTimeout act = do
              asyncs <- act 
              let loop = do
                    best'0 <- snd `fmap` readIORef bestRef
                    delay
                    best'1 <- snd `fmap` readIORef bestRef
                    if best'0 == best'1 
                     then do
                       printTL "UNABLE TO IMPROVE, TIMEOUT"
                       readIORef bestRef 
                     else loop
                  delay = Timeout.threadDelay (60 # Second)
              timeoutAsync <- async loop
              return (timeoutAsync:asyncs)

                                      
--    (_, best) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronRandomSearch (searchCB bestRef) thrG thr fn constraints)) [1..threads])
--    (_, bestLocal) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronLocalSearch (searchCB bestRef) bestRef localSearch thrL thr fn constraints)) [1..threads])
    (dbn, _) <- parseNetFromFile `fmap` readFile fn 
    let constraintsPacked = map packConstraint $ concatMap (uncurry generateConstraintsSimple) constraints
        packConstraint c = fmap packGame c
        packGame game = computeTNetworkSigmoid dbn $ reprToNN $ toRepr game

    (_, _best) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronRandomReprSearch (searchCB bestRef) thrG thr constraintsPacked)) [1..threads])
    (_, bestLocal) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronLocalReprSearch (searchCB bestRef) bestRef localSearch thrL thr constraintsPacked)) [1..threads])

    putStrLnTL $ printf "FINAL SCORE %s" (show bestLocal)

    printTL "BEGIN EVALUATE"

    let llNetwork = uncurry mkTNetwork (fst bestLocal)
        evalNetwork = appendNetwork dbn llNetwork
        
    agSmpl <- mkTimed "simple" evalNetwork :: IO (AgentTrace AgentSimple)
    agTree <- mkTimed "tree" (evalNetwork, 4) :: IO (AgentTrace AgentGameTree)
    
    agRnd <- mkTimed "random" () :: IO (AgentTrace AgentRandom)
    agMTC <- mkTimed "mcts" 5 :: IO (AgentTrace AgentMCTS)

--     let reportWin ag ag2 pl = do
--               winRef <- newIORef (0,0)
--               parWorkThreads evaluateWinnersCount (\ cnt -> sampleGameDepthCount ag ag2 cnt (calculateWinnersPCT pl winRef))
--               reportWinnersPCT ag ag2 pl winRef
    putStrLnTL "======================================================================================"
    -- reportWin agSmpl agRnd P1
    reportWin agSmpl agMTC P1
    -- reportWin agTree agRnd P1
    reportWin agTree agMTC P1
    putStrLnTL "======================================================================================"

    return ()

  printTL "Source file changed, exiting"
  