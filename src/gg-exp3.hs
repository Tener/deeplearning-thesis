{-# LANGUAGE BangPatterns #-}

module Main where

import BreakthroughGame
import Board

import GenericGameExperiments
import GenericGame
import AgentGeneric
import ConstraintsGeneric
import MinimalNN
import NeuralNets (parseNetFromFile)

import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Text.Printf
import System.IO
import System.Directory
import System.FilePath

import qualified Control.Concurrent.Timeout as Timeout
import Data.Timeout

type MyGameA = Abalone
type MyGameB = Breakthrough
type MyGame = MyGameB

someGame :: MyGame
someGame = freshGameDefaultParams

printFlush p = print p >> hFlush stdout

constraintsCacheFilename = ("tmp-data" </> "cache" </> "constraints-" ++ gameName someGame <.> "txt")

genConstraints = do
  cR <- newIORef []
  let addConstraints cs = atomicModifyIORef cR (\ !old -> ((cs:old),()))
  sampleRandomGamesCount 150 0.01 (\ g -> do
                                     cs <- generateConstraintsMCTS 1000 (g :: MyGame)
                                     addConstraints cs
                                     printFlush =<< length `fmap` readIORef cR
                                  )
  c <- readIORef cR
  createDirectoryIfMissing True (takeDirectory constraintsCacheFilename)
  writeFile constraintsCacheFilename (show c)
  return c

genConstraintsCached = do
  b <- doesFileExist constraintsCacheFilename
  if b then read `fmap` readFile constraintsCacheFilename
       else genConstraints


evaluateWinnersCount = 1000 :: Int

calculateWinnersPCT ref g _dep = when (winner (g :: MyGame) == Just P1) (atomicModifyIORef ref (\ !v -> ((v+1),())))
reportWinnersPCT ref = do
  winCount <- readIORef ref
  let winPCT = 100 * ((fromIntegral winCount) / (fromIntegral evaluateWinnersCount)) :: Double
  putStrLn "======================================================================================"
  putStrLn (printf "P1 won in %d matches, win percentage: %f%%" (winCount :: Int) winPCT :: String)
  putStrLn "======================================================================================"
  hFlush stdout
  -- threadDelay (10000 * (10^6))

main = do
  hSetBuffering stdout NoBuffering

  printFlush "Constraint generation"
  let useConstraintCache = True
  constraints <- if useConstraintCache then genConstraintsCached else genConstraints 

  let fnBr = return "tmp-data/irlfjflptuwgzpqzejrd/dbn.txt"
      fnAb = return "tmp-data/mlubiwjdnaaovrlgsqxu/dbn.txt"
      fnTN = sampleGamesTrainNetwork (freshGameDefaultParams :: MyGame) 100000 0.1

      isAbalone = toRepr someGame == toRepr (freshGameDefaultParams :: Abalone)
      isBreakthrough = toRepr someGame == toRepr (freshGameDefaultParams :: Breakthrough)

      useCachedDBN = False

  printFlush "DBN read/train"
  fn <- case (isAbalone, isBreakthrough, useCachedDBN) of
          (True, False, True) -> fnAb
          (False, True, True) -> fnBr
          (_, _, _) -> fnTN

  let searchCB ref = (\ new@(bnNew,bsNew,acNew) -> do
                        let newTrim = (bnNew, bsNew)
                        updated <- atomicModifyIORef 
                                    ref (\ old@(bnOld, !bsOld) -> do
                                                       if bsOld < bsNew
                                                        then (newTrim, True)
                                                        else (old,False))
                        when updated (printFlush newTrim >> acNew)
                     )

  printFlush "forever train last layer network & evaluate"
  forever $ do
    threads <- getNumCapabilities
    bestRef <- newIORef (undefined, neginf)
    let thrG = 0.80
        thrL = 0.90
        localSearch = 0.18

        withTimeout act = do
              asyncs <- act 
              let loop = do
                    best'0 <- snd `fmap` readIORef bestRef 
                    delay
                    best'1 <- snd `fmap` readIORef bestRef 
                    if best'0 == best'1 
                     then do
                       printFlush "UNABLE TO IMPROVE, TIMEOUT"
                       readIORef bestRef 
                     else loop
                  delay = Timeout.threadDelay (1 # Minute)
              timeoutAsync <- async loop
              return (timeoutAsync:asyncs)

                                      
    (_, best) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronRandomSearch (searchCB bestRef) thrG thr fn constraints)) [1..threads])
    (_, bestLocal) <- waitAnyCancel =<< withTimeout (mapM (\ thr -> async (singleNeuronLocalSearch (searchCB bestRef) bestRef localSearch thrL thr fn constraints)) [1..threads])

    let llNetwork = uncurry mkTNetwork (fst bestLocal)

    printFlush "BEGIN EVALUATE"
        
    (dbn, _) <- parseNetFromFile `fmap` readFile fn 
    myTrainedAgent <- mkAgent (appendNetwork dbn llNetwork) :: IO AgentSimple
    agRnd <- mkAgent () :: IO AgentRandom

    winRef <- newIORef 0
    sampleGameDepthCount myTrainedAgent agRnd evaluateWinnersCount (calculateWinnersPCT winRef)
    reportWinnersPCT winRef
    return ()

  return ()

  