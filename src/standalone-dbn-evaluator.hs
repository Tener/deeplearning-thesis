module Main where

import ThreadLocal
import GenericGameExperiments
import AgentGeneric
import GenericGame
import MinimalNN
import MCTS

import Control.Applicative
import System.Environment


main :: IO ()
main = mapM_ evaluateDBN'Short =<< getArgs

-- TODO: make it work with GNetwork
-- assertType :: a -> a -> ()
-- assertType _ _ = ()

evaluateDBN'Short :: FilePath -> IO ()
evaluateDBN'Short fn = runThrLocMainIO $ do
  dbn <- read <$> readFile fn
  -- let () = assertType dbn nn
  -- agSmpl <- mkAgent (dbn :: TNetwork) :: IO AgentSimple
  agSmpl <- mkTimed "simple" (dbn :: TNetwork) :: IO (AgentTrace (AgentSimple TNetwork))
  agTree <- mkTimed "tree" (dbn, 4) :: IO (AgentTrace (AgentGameTree TNetwork))
  agMCT <- mkTimed "mcts" 75 :: IO (AgentTrace AgentMCTS)
  agSmpl'bare <- mkAgent (dbn :: TNetwork) :: IO (AgentSimple TNetwork)
  agMCTEv <- mkTimed "mcts'eval" (75, agSmpl'bare) :: IO (AgentTrace (AgentMCTS'Eval (AgentSimple TNetwork)))
  agMCTNet <- mkTimed "mctNet" (2, 50, dbn) :: IO (AgentTrace (AgentParMCTS (AgentSimple TNetwork)))

  agRnd <- mkAgent () :: IO AgentRandom
  putStrLnTL "======================================================================================"
  putStrLnTL ("FN=" ++ fn)
  _ <- reportWinCount 10 agMCT agSmpl  P2
  _ <- reportWinCount 10 agSmpl  agMCT P1
  _ <- reportWinCount 10 agTree  agMCT P1
  _ <- reportWinCount 10 agMCTEv agMCT P1
  _ <- reportWinCount 10 agMCT agTree  P2
  _ <- reportWinCount 10 agMCT agMCTEv P2
  -- _ <- reportWinCount 20 agMCTNet agMCT P1

  putStrLnTL "======================================================================================"


evaluateDBN :: FilePath -> IO ()
evaluateDBN fn = runThrLocMainIO $ do
  dbn <- read <$> readFile fn
  agSmpl <- mkTimed "simple" (dbn :: TNetwork) :: IO (AgentTrace (AgentSimple TNetwork))
  agTree <- mkTimed "tree" (dbn, 4) :: IO (AgentTrace (AgentGameTree TNetwork))
  agMCT <- mkTimed "mcts" 50 :: IO (AgentTrace AgentMCTS)
  agMCTNet <- mkTimed "mctNet" (2, 5, dbn) :: IO (AgentTrace (AgentParMCTS (AgentSimple TNetwork)))
  agMCTS'Tree <- mkTimed "mcts'Tree" (2, 5, dbn) :: IO (AgentTrace (AgentProperMCTS'Tree (AgentSimple TNetwork)))

  putStrLnTL "======================================================================================"
  putStrLnTL ("FN=" ++ fn)
  putStrLnTL "======================================================================================"
  w0 <- reportWin agSmpl agMCT P1
  w1 <- reportWin agTree agMCT P1
  w2 <- reportWin agMCTNet agMCT P1
  w3 <- reportWin agMCTS'Tree agMCT P1
  putStrLnTL "======================================================================================"
  print [w0,w1,w2,w3]
  putStrLnTL "======================================================================================"
