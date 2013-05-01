{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module CommonDatatypes where

import Board
import Data.Tree.Game_tree.Game_tree as GTree

data BBoard = BBoard Board -- board when the black has the move
data WBoard = WBoard Board -- board when the white has the move


class BoardLike a where
    unwrap :: a -> Board
instance BoardLike BBoard where
    unwrap (BBoard brd) = brd
instance BoardLike WBoard where
    unwrap (WBoard brd) = brd

class Tagged a b | a -> b, b -> a where
    retag :: a -> b
    morph :: a -> b
instance Tagged WBoard BBoard where 
    retag (WBoard brd) = BBoard brd
    morph (WBoard brd) = BBoard (negateBoard brd)
instance Tagged BBoard WBoard where
    retag (BBoard brd) = WBoard brd
    morph (BBoard brd) = WBoard (negateBoard brd)

class Agent a where
    mkAgent :: Color -> IO a
    setColor :: Color -> a -> a
    setColor _ _ = error "setColor: not implemented (by default)"
    makeMove :: a -> BBoard -> IO WBoard
    getAgentName :: a -> String
    getAgentName _ = error "getAgentName: not implemented (by default)"
    evaluateBoard :: a -> BBoard -> [(String,String)]
    evaluateBoard _ _ = error "evaluateBoard: not implemented (by default)"

data GameState = GameState { gtBoard :: BBoard
                           , gtEvalSelf :: GameState -> Int
                           , gtColorBase :: Color -- względem którego gracza zwracamy wynik
                           , gtColorNow :: Color  -- który gracz teraz się rusza
                           } 

instance GTree.Game_tree GameState where
    is_terminal gst = Board.isFinished (unwrap $ gtBoard gst)
    node_value gst = (gtEvalSelf gst) gst
    children (GameState (BBoard brd0) eval colB colN) | isFinished brd0 = []
                                                      | otherwise = [ GameState (mkBBoard colN brd) eval colB (Board.negColor $ colN) | brd <- Board.getMoves colN brd0 ]

-- | mkBBoard previous'color board'now
mkBBoard :: Color -> Board -> BBoard
mkBBoard White brd = BBoard brd
mkBBoard Black brd = BBoard (negateBoard brd)

-- getMovesFixColor :: Color -> BBoard -> [Board]
getMovesFixColor :: Color -> BBoard -> [Board]
getMovesFixColor col brd@(BBoard _) = case col of
                             White -> getMoves col (unwrap $ morph brd)
                             Black -> getMoves col (unwrap brd)
