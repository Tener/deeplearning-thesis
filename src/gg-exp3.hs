{-# LANGUAGE BangPatterns #-}

module Main where

import BreakthroughGame
import Board

import GenericGameExperiments
import GenericGame
import AgentGeneric
import ConstraintsGeneric

import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

type MyGame = Breakthrough

someGame :: MyGame
someGame = freshGameDefaultParams

main = do
  cR <- newIORef []
  let addConstraints cs = atomicModifyIORef cR (\ !old -> ((cs:old),()))

  sampleRandomGamesCount 10 0.001 (\ g -> do
                                     print "gen start"
                                     cs <- generateConstraintsMCTS 1000 (g :: MyGame)
                                     addConstraints cs
                                     print =<< length `fmap` readIORef cR
                                  )

  constraints <- readIORef cR
  
  fn <- sampleGamesTrainNetwork (freshGameDefaultParams :: MyGame) 100000 0.1

  let searchCB = (\ (a,b,c) -> print (a,b) >> c)

  forever $ do
    threads <- getNumCapabilities
    (_, best) <- waitAnyCancel =<< (mapM (\ thr -> async (singleNeuronRandomSearch searchCB 0.7 thr fn constraints)) [1..threads])
    bestRef <- newIORef best
    (_, bestLocal) <- waitAnyCancel =<< (mapM (\ thr -> async (singleNeuronLocalSearch searchCB bestRef 0.04 0.8 thr fn constraints)) [1..threads])
    return ()

  return ()

  