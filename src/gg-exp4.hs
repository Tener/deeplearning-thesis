{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (print, putStr, putStrLn)

import GenericGameExperiments
import GenericGame
import AgentGeneric
import ThreadLocal

import Data.List(tails)

data Ag where Ag :: (Agent2 a) => a -> Ag

main :: IO ()
main = runThrLocMainIO $ do
         agRnd <- mkAgent () :: IO AgentRandom
         agMTCs <- mapM (\ d -> mkTimed ("mtcs_" ++ show d) d :: IO (AgentTrace AgentMCTS)) [1,5,10,15,30,100,200,300]
         let agents = tail $ ((Ag agRnd) : (map Ag agMTCs))
         sequence_ [do
                     printTL (agentName ag1, agentName ag2)
                     reportWin ag1 ag2 P1
                    | ((Ag ag1):ag'rest) <- tails agents, (Ag ag2) <- ag'rest]
