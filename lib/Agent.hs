module Agent where

import Board
import CairoRender

import Control.Monad (when)
import Text.Printf
import System.Random.MWC

type MakeMove = Board -> IO Board
data AgentSimple = AgentSimple { gen :: GenIO, color :: Color }

mkAgentSimple :: Color -> IO AgentSimple
mkAgentSimple col = do
  g <- withSystemRandom $ asGenIO $ return
  return (AgentSimple g col)

makeMove :: AgentSimple -> MakeMove
makeMove agent brd = do
  let moves = getMoves (color agent) brd
  case moves of
    [] -> do
      print "Stuck, cant do anything."
      saveBoard brd "board-stuck.svg"
      return brd
    _ -> do
      pick <- uniformR (0, length moves - 1) (gen agent)
      return (moves !! pick)

play :: Int -> Int -> Board -> AgentSimple -> AgentSimple -> IO Board
play cutoff cnt brd a'fst a'snd | (isFinished brd || cnt == cutoff) = do
  putStrLn (printf "Game is finished after %d moves!" cnt)
  putStrLn "Winner:"
  print (getWinner brd)
  -- saveBoard brd "finished-board.svg"
  return brd
  

                     | otherwise = do
  -- when (cnt `mod` 1000 == 0) (saveBoard brd (printf "playing-board-%06d.svg" cnt))
  brd'new <- makeMove a'fst brd
  play cutoff (cnt+1) brd'new a'snd a'fst


game :: Int -> IO Board
game cutoff = do
  ag'black <- mkAgentSimple Black
  ag'white <- mkAgentSimple White

  play cutoff 1 starting'board'default ag'white ag'black

