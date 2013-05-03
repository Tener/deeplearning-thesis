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

type MyGameA = Abalone
type MyGameB = Breakthrough
type MyGame = MyGameB

someGame :: MyGame
someGame = freshGameDefaultParams

genConstraints = do
  cR <- newIORef []
  let addConstraints cs = atomicModifyIORef cR (\ !old -> ((cs:old),()))
  sampleRandomGamesCount 10 0.01 (\ g -> do
                                     print "gen start"
                                     cs <- generateConstraintsMCTS 100 (g :: MyGame)
                                     addConstraints cs
                                     print =<< length `fmap` readIORef cR
                                  )
  readIORef cR

evaluateWinnersCount = 1000 :: Int

calculateWinnersPCT ref g _dep = when (winner (g :: MyGame) == Just P1) (atomicModifyIORef ref (\ !v -> ((v+1),())))
reportWinnersPCT ref = do
  winCount <- readIORef ref
  let winPCT = 100 * ((fromIntegral winCount) / (fromIntegral evaluateWinnersCount)) :: Double
  putStrLn (printf "P1 won in %d matches, win percentage: %f%%" (winCount :: Int) winPCT :: String)

main = do
  constraints <- genConstraints

  let fnBr = return "tmp-data/irlfjflptuwgzpqzejrd/dbn.txt"
      fnAb = return "tmp-data/mlubiwjdnaaovrlgsqxu/dbn.txt"
      fnTN = sampleGamesTrainNetwork (freshGameDefaultParams :: MyGame) 100000 0.1

      isAbalone = toRepr someGame == toRepr (freshGameDefaultParams :: Abalone)
      isBreakthrough = toRepr someGame == toRepr (freshGameDefaultParams :: Breakthrough)

      useCachedDBN = True

  fn <- case (isAbalone, isBreakthrough, useCachedDBN) of
          (True, False, True) -> fnAb
          (False, True, True) -> fnBr
          (_, _, _) -> fnTN

  let searchCB ref = (\ new@(bnNew,bsNew,acNew) -> do
                        print (a,b) >> c)

  forever $ do
    threads <- getNumCapabilities
    let thrG = 0.8
    (_, best) <- waitAnyCancel =<< (mapM (\ thr -> async (singleNeuronRandomSearch searchCB thrG thr fn constraints)) [1..threads])
    bestRef <- newIORef best
    (_, bestLocal) <- waitAnyCancel =<< (mapM (\ thr -> async (singleNeuronLocalSearch searchCB bestRef 0.04 (thrG+0.05) thr fn constraints)) [1..threads])

    let llNetwork = uncurry mkTNetwork (fst bestLocal)
    (dbn, _) <- parseNetFromFile `fmap` readFile fn 
    myTrainedAgent <- mkAgent (dbn, llNetwork) :: IO AgentSimpleLL
    agRnd <- mkAgent () :: IO AgentRandom

    winRef <- newIORef 0
    sampleGameDepthCount myTrainedAgent agRnd evaluateWinnersCount (calculateWinnersPCT winRef)
    reportWinnersPCT winRef
    return ()

  return ()

  