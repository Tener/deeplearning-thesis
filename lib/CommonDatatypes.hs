module CommonDatatypes where

import Board
import Data.Tree.Game_tree.Game_tree as GTree

type MakeMove = Board -> IO Board

class Agent a where
    mkAgent :: Color -> IO a
    setColor :: Color -> a -> a
    makeMove :: a -> MakeMove
    getAgentName :: a -> String
    evaluateBoard :: a -> Board -> [(String,String)]

data GameState = GameState { gtBoard :: Board
                           , gtEvalSelf :: GameState -> Int
                           , gtColorBase :: Color -- względem którego gracza zwracamy wynik
                           , gtColorNow :: Color  -- który gracz teraz się rusza
                           } 

instance GTree.Game_tree GameState where
    is_terminal gst = Board.isFinished (gtBoard gst)
    node_value gst = (gtEvalSelf gst) gst
    children (GameState brd0 eval colB colN) | isFinished brd0 = []
                                             | otherwise = [ GameState brd eval colB (Board.negColor $ colN) | brd <- Board.getMoves colN brd0 ]
