{-# LANGUAGE CPP #-}

module TestBoards

(module TestBoards,
 module Board,
 module Agent,
#ifdef CAIRO
 view,
#endif
 nub
 
)

where

import Agent
import Board
import CairoRender
import Data.List (nub, sort)

import Control.Applicative
import Control.Monad
import Test.QuickCheck

newtype Small = Small {fromSmall :: Int} deriving Show
instance Arbitrary Small where arbitrary = Small . (`mod` 3) <$> arbitrary
instance Arbitrary Board where arbitrary = (denseReprToBoard . map fromSmall) <$> replicateM 61 arbitrary
instance Arbitrary Color where arbitrary = bool Black White <$> arbitrary

bool t f b = if b then t else f

#ifndef CAIRO
view :: Board -> IO ()
view = print -- otherwise defined in CairoRender
#endif

board'empty :: Board
board'empty = denseReprToBoard (take 61 (cycle [0]))

-- | b0.
--
-- >>> length $ getMoves White b0
-- 6
-- >>> getMoves Black b0
-- []

b0 = foldl (flip putBall) board'empty 
     [((0,0),White)]


-- | b1.
--
-- >>> length $ getMoves White b1
-- 3
-- >>> length $ getMoves Black b1
-- 0

b1 = foldl (flip putBall) board'empty 
     [((-4,0),White)]



-- | b2.
--
-- >>> length $ getMoves White b2
-- 2
-- >>> length $ getMoves Black b2
-- 15

b2 = foldl (flip putBall) board'empty 
     [((-4,0),White)
     ,((-3,0),Black)
     ,((-2,0),Black)
     ]

-- | b3.
--
-- >>> length $ getMoves White b3
-- 2
-- >>> length $ getMoves Black b3
-- 29

b3 = foldl (flip putBall) board'empty 
     [((-4,0),White)
     ,((-3,0),Black)
     ,((-2,0),Black)
     ,((-1,0),Black)
     ]

-- | b4.
--
-- >>> length $ getMoves White b4
-- 8
-- >>> length $ getMoves Black b4
-- 28

b4 = foldl (flip putBall) board'empty 
     [((-4,0),White)
     ,((-3,0),White)
     ,((-2,0),Black)
     ,((-1,0),Black)
     ,((0,0),Black)
     ]


-- $other
--
-- prop> \brd -> let mvs = getMoves White brd in (length mvs) == (length $ nub mvs)
-- prop> \brd -> let mvs = getMoves Black brd in (length mvs) == (length $ nub mvs)
-- prop> \brd col -> let mvs = getMoves col brd in (length mvs) == (length $ nub mvs)
