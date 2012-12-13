module Agent where

import Board
import CairoRender

import Control.Monad (when)
import Text.Printf
import System.Random.MWC
import Data.Ord
import Data.List (sortBy)

import qualified Math.Geometry.GridMap as GridMap


type MakeMove = Board -> IO Board
data AgentRandom = AgentRandom { gen :: GenIO, color :: Color }
data AgentSimple = AgentSimple { gen's :: GenIO, color's :: Color }


class Agent a where
    mkAgent :: Color -> IO a
    makeMove :: a -> MakeMove

instance Agent AgentRandom where
    mkAgent col = do
      g <- withSystemRandom $ asGenIO $ return
      return (AgentRandom g col)
    makeMove agent brd = do
      let moves = getMoves (color agent) brd
      case moves of
        [] -> do
          print "Stuck, cant do anything."
          saveBoard brd "board-stuck.svg"
          return brd
        _ -> do
          pick <- uniformR (0, length moves - 1) (gen agent)
          let chosen = (moves !! pick)
          print ("random",chosen)
          return chosen

instance Agent AgentSimple where
    mkAgent col = do
      g <- withSystemRandom $ asGenIO $ return
      return (AgentSimple g col)
    makeMove agent brd = do
      let moves = getMoves (color's agent) brd
      case moves of
        [] -> do
          print "Stuck, cant do anything."
          saveBoard brd "board-stuck.svg"
          return brd
        _ -> do
          let maxdepth = 2
          print (length moves)
          bare <- mapM (negmax maxdepth (color's agent)) moves
          let weighted = sortBy (comparing fst) $ zip bare moves
              bestScore = fst $ last $ weighted
              best = map snd $ filter ((==bestScore).fst) weighted
          putStrLn (printf "Color: %s, score: %f" (show (color's agent)) bestScore)
          pick <- uniformR (0, length best - 1) (gen's agent)
          return (best !! pick)

play :: (Agent a, Agent b) => Int -> Int -> Board -> a -> b -> IO Board
play cutoff cnt brd a'fst a'snd | (isFinished brd || cnt == cutoff) = do
  putStrLn (printf "Game is finished after %d moves!" cnt)
  putStrLn "Winner:"
  print (getWinner brd)
  -- saveBoard brd "finished-board.svg"
  return brd

                     | otherwise = do
  -- when (cnt `mod` 1000 == 0) (saveBoard brd (printf "playing-board-%06d.svg" cnt))
  brd'new <- makeMove a'fst brd
  print brd'new
  play cutoff (cnt+1) brd'new a'snd a'fst


game :: Int -> IO Board
game cutoff = do
  ag'black <- mkAgent Black
  ag'white <- mkAgent White

  play cutoff 1 starting'board'default (ag'white :: AgentRandom) (ag'black :: AgentRandom)

negmax :: Int -> Color -> Board -> IO Double
negmax 0 col brd = do
  let val = (evalBoard col brd)
--  putStr "." -- print ("negmax finish", val)
  return val
-- negmax n col brd -- | isFinished brd = evalBoard col brd
--                  | otherwise = do
negmax n col brd = do
  putStr $ show n
  putStr " "
  vals <- mapM (negmax (n-1) (negColor col)) (getMoves col brd)
  return (maximum ((-1/0) : map negate vals))

-- ocenia planszę według heurystyki
evalBoard :: Color -> Board -> Double
evalBoard col brd = 
    case getWinner brd of 
      Just col'win -> if col'win == col then 1000 else -1000
      Nothing -> fromIntegral $ marbleCount col brd - marbleCount (negColor col) brd

