module Agent where

import Board
import CairoRender
import CommonDatatypes

import Control.Monad (when)
import Text.Printf
import System.Random.MWC
import Data.Ord
import Data.List (sortBy)

import qualified Data.Tree.Game_tree.Negascout as GTreeAlgo

-- plansza i aktualny kolor gracza

data AgentRandom = AgentRandom { gen :: GenIO, color :: Color }
data AgentSimple = AgentSimple { gen's :: GenIO, color's :: Color }
data AgentGameTree = AgentGameTree GenIO Color

instance Agent AgentGameTree where
    mkAgent col = do
      g <- withSystemRandom $ asGenIO $ return
      return (AgentGameTree g col)
    makeMove _agent@(AgentGameTree _gen col) brd = do
      let gst = GameState brd (\ g -> evalBoardB (gtColorNow g) (gtBoard g)) col col
          depth = 4
          (princ, score) = GTreeAlgo.negascout gst depth
      when (score /= 0) (print ("gtree",score,col))
      return (morph $ gtBoard $ head $ tail $ princ)
      
instance Agent AgentRandom where
    mkAgent col = do
      g <- withSystemRandom $ asGenIO $ return
      return (AgentRandom g col)
    makeMove agent brd = do
      let moves = getMovesFixColor (color agent) brd
      case moves of
        [] -> do
          print "Stuck, cant do anything."
          saveBoard (unwrap brd) "board-stuck.svg"
          return (morph brd :: WBoard)
        _ -> do
          pick <- uniformR (0, length moves - 1) (gen agent)
          let chosen = (moves !! pick)
          -- print ("random",chosen)
          return (WBoard chosen)

instance Agent AgentSimple where
    mkAgent col = do
      g <- withSystemRandom $ asGenIO $ return
      return (AgentSimple g col)
    makeMove agent brd = do
      let moves = getMovesFixColor (color's agent) brd
      case moves of
        [] -> do
          print "Stuck, cant do anything."
          saveBoard (unwrap brd) "board-stuck.svg"
          return (morph brd)
        _ -> do
          let maxdepth = 2
          print (length moves)
          bare <- mapM (negmax maxdepth (color's agent)) moves
          let weighted = sortBy (comparing fst) $ zip bare moves
              bestScore = fst $ last $ weighted
              best = map snd $ filter ((==bestScore).fst) weighted
          putStrLn (printf "Color: %s, score: %f" (show (color's agent)) bestScore)
          pick <- uniformR (0, length best - 1) (gen's agent)
          return (WBoard (best !! pick))

negmax :: Int -> Color -> Board -> IO Double
negmax 0 col brd = do
  let val = (evalBoard col brd)
--  putStr "." -- print ("negmax finish", val)
  return val
-- negmax n col brd -- | isFinished brd = evalBoard col brd
--                  | otherwise = do
negmax n col brd = do
  vals <- mapM (negmax (n-1) (negColor col)) (getMoves col brd)
  let val = (maximum ((-1/0) : map negate vals))
  return $! val

-- ocenia planszę według heurystyki
evalBoard :: Color -> Board -> Double
evalBoard col brd = 
    case getWinner brd of 
      Just col'win -> if col'win == col then 1000 else -1000
      Nothing -> fromIntegral $ marbleCount col brd - marbleCount (negColor col) brd    

evalBoardI :: Color -> Board -> Int
evalBoardI col brd = 
    case getWinner brd of 
      Just col'win -> if col'win == col then 1000 else -1000
      Nothing -> fromIntegral $ marbleCount col brd - marbleCount (negColor col) brd

evalBoardB :: Color -> BBoard -> Int
evalBoardB Black (BBoard brd) = evalBoardI Black brd
evalBoardB White (BBoard brd) = evalBoardI White (negateBoard brd)


--------------------------
