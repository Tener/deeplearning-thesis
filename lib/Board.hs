{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-} 

module Board where

import Prelude hiding (lookup)

import Data.List (nub, sort, group, intercalate)
import System.IO

import Math.Geometry.Grid
import Math.Geometry.GridMap as GridMap hiding (map) 

type Position = (Int,Int)
type Board = GridMap HexHexGrid Position (Maybe Color)
type Direction = (Position, Position)

-- instance Ord Board where
--     compare brd0 brd1 = compare (GridMap.toList brd0) (GridMap.toList brd1)

{-

-- ruch prosty bez spychania

.    ke
..   kke
...  kkke

-- ruchy proste spychające

..*  kkoe
...* kkkoe
...** kkkooe

-- ruchy na ukos: 1, 2 i 3 kulki
. ke2
.. ke2
... 

-}

---- board regexp

data Color = Black | White deriving (Eq, Ord, Read, Show)

data Field = Empty | Death | Ball | Opponent deriving (Eq, Ord, Read, Show)
data Match = And {-# UNPACK #-} !Match {-# UNPACK #-} !Match 
           | Or {-# UNPACK #-} !Match {-# UNPACK #-} !Match
           | This {-# UNPACK #-} !Field
           | Next {-# UNPACK #-} !Match
           | Diag {-# UNPACK #-} !Match
             deriving (Eq, Ord, Read, Show)

data Setter = SetHere Field | SetNext Setter | SetDiag Setter
             deriving (Eq, Ord, Read, Show)

type Move = [Setter]

-- negacja koloru
negColor Black = White
negColor White = Black

{-# INLINE rules #-}
rules = [ (here,    move'here) 
        , (here2,   move'here2) 
        , (here3,   move'here3) 
        , (shove2,  move'shove2) 
        , (shove31, move'shove31) 
        , (shove32, move'shove32) 
        , (diag,    move'diag) 
        , (diag2,   move'diag2) 
        , (diag3,   move'diag3)] 

    where
       -- mini DSL
       mnext :: Int -> Match -> Match
       mnext 0 what = what
       mnext n what = Next (mnext (n-1) what)

       mball = This Ball
       mdball = Diag mball
       mopponent = This Opponent
       mempty = This Empty
       mdeath = This Death

       -- ruchy proste
       here = And (This Ball) (Next (This Empty))
       here2 = And (This Ball) (Next here)
       here3 = And (This Ball) (Next here2)
        
       -- ruchy proste spychające
        
       deadOrEmpty = (Or (This Death) (This Empty))

#if 1
       shove2 = mball `And` mnext 1 (mball `And` mnext 1 (mopponent `And` mnext 1 deadOrEmpty))
       shove31 = mball `And` mnext 1 (mball `And` mnext 1 (mball `And` mnext 1 (mopponent `And` mnext 1 deadOrEmpty)))
       shove32 = mball `And` mnext 1 (mball `And` mnext 1 (mball `And` mnext 1 (mopponent `And` mnext 1 (mopponent `And` mnext 1 deadOrEmpty))))
#else
       shove2 = mball `And` mnext 1 mball `And` mnext 2 mopponent `And` mnext 3 deadOrEmpty
       shove31 = mball `And` mnext 1 mball `And` mnext 2 mball `And` mnext 3 mopponent `And` mnext 4 deadOrEmpty
       shove32 = mball `And` mnext 1 mball `And` mnext 2 mball `And` mnext 3 mopponent `And` mnext 4 mopponent `And` mnext 5 deadOrEmpty
#endif
        
       -- ruchy na skos
       diag = And (This Ball) (Diag (This Empty))
       diag2 = And diag (Next diag)
       diag3 = And diag2 (Next (Next diag))

       ---- same ruchy - tzn. funkcje modyfikujące planszę

       next :: Int -> Setter -> Setter
       next 0 what = what
       next n what = SetNext (next (n-1) what)

       ball = SetHere Ball
       dball = SetDiag ball
       opponent = SetHere Opponent
       empty = SetHere Empty

       move'here    = [empty, next 1 ball]   
       move'here2   = [empty, next 2 ball]
       move'here3   = [empty, next 3 ball]
       move'shove2  = [empty, next 2 ball, next 3 opponent]
       move'shove31 = [empty, next 3 ball, next 4 opponent]
       move'shove32 = [empty, next 3 ball, next 5 opponent]
       move'diag    = [empty, 
                       dball]
       move'diag2   = [empty, next 1 empty, 
                       dball, next 1 dball]
       move'diag3   = [empty, next 1 empty, next 2 empty, 
                       dball, next 1 dball, next 2 dball]


--fix: old and non-maintained. keep DRY: check out getMoves
--test col brd = [ let b = runMove col brd idx way setter
--              in (idx, way, rule, b) | (idx,val) <- (toList brd), val == Just col, way <- ways, (rule,setter) <- rules, tryMatch col brd idx way rule]

nubOrd xs = map head $ group $ sort xs

getMoves col brd = 
                   [ runMove col brd idx way setter | 
                     (rule,setter) <- rules
                   , (idx,val) <- toList brd
                   , val == Just col
                   , way <- ways
                   , tryMatch col brd idx way rule
                   ]


marbleCount col brd = length $ filter (== (Just col)) $ Prelude.map snd $ GridMap.toList brd

isFinished :: Board -> Bool
isFinished brd = getWinner brd /= Nothing

getWinner :: Board -> Maybe Color
getWinner brd = case () of
                  _ | marbleCount White brd <= 14-6 -> Just Black
                    | marbleCount Black brd <= 14-6 -> Just White
                    | otherwise -> Nothing

-- score board
scoreBoard brd = (flt White, flt Black)
    where 
      lst = GridMap.toList brd
      flt c = length $ filter (\ (_, v) -> v == Just c) lst

-- wykonaj ruch
runMove :: Color -> Board -> Position -> Direction -> Move -> Board
runMove col brd pos dir move = foldl (\ board setter -> runSetter col board pos dir setter) brd move

runSetter side brd pos dir@(dnext, ddiag) setter = 
    case setter of
      (SetHere fld) -> case fld of
                         Ball -> adjust (const (Just side)) pos brd 
                         Opponent -> adjust (const (Just (negColor side))) pos brd
                         Empty -> adjust (const Nothing) pos brd
                         Death -> error "Cant set DEATH anywhere!"
      (SetDiag set) -> runSetter side brd pos'diag dir set
      (SetNext set) -> runSetter side brd pos'next dir set

    where -- TODO: be DRY, dont copy-paste that code from below
      pos'next  = (fst pos + fst dnext, snd pos + snd dnext)
      pos'diag = (fst pos + fst ddiag, snd pos + snd ddiag)

-- wypróbuj ruch
tryMatch :: Color -> Board -> Position -> Direction -> Match -> Bool
tryMatch side brd pos dir@(dnext, ddiag) match = 
    case match of
      And m1 m2 -> (tryMatch side brd pos dir m1) && (tryMatch side brd pos dir m2)
      Or m1 m2 -> (tryMatch side brd pos dir m1) || (tryMatch side brd pos dir m2)
      Next m -> tryMatch side brd pos'next dir m
      Diag m -> tryMatch side brd pos'diag dir m
      This f -> case f of
                  Ball -> lookup pos brd == (Just (Just side))
                  Opponent -> lookup pos brd == (Just (Just (negColor side)))
                  Empty -> lookup pos brd == Just Nothing
                  Death -> lookup pos brd == Nothing
    where
      pos'next  = (fst pos + fst dnext, snd pos + snd dnext)
      pos'diag = (fst pos + fst ddiag, snd pos + snd ddiag)


-- http://en.wikipedia.org/wiki/File:Abalone_standard.svg
starting'board'default :: Board
starting'board'default = lazyGridMap fresh'grid balls
    where
      balls = [e,e,e,b,b,
               e,e,e,e,b,b,
               e,e,e,e,b,b,b,
               w,e,e,e,e,b,b,b,
               w,w,w,e,e,e,b,b,b,
               w,w,w,e,e,e,e,b,
               w,w,w,e,e,e,e,
               w,w,e,e,e,e,
               w,w,e,e,e]

      b = Just Black
      w = Just White
      e = Nothing

-- http://en.wikipedia.org/wiki/File:Abalone_belgian.svg
starting'board'belgianDaisy :: Board
starting'board'belgianDaisy = lazyGridMap fresh'grid balls
    where
      balls = [        e,e,e,w,w,
                     e,e,e,w,w,w,
                   e,e,e,e,w,w,e,
                 b,b,e,e,e,e,b,b,
               b,b,b,e,e,e,b,b,b,
                 b,b,e,e,e,e,b,b,
                   e,w,w,e,e,e,e,
                     w,w,w,e,e,e,
                       w,w,e,e,e]

      b = Just Black
      w = Just White
      e = Nothing

starting'board'death :: Board
starting'board'death = lazyGridMap fresh'grid balls
    where
      balls = [        e,e,e,e,e,
                     e,e,e,e,e,e,
                   e,e,e,e,e,e,e,
                 e,e,e,e,e,e,w,w,
               e,e,e,e,b,w,w,w,b,
                 e,e,e,e,e,e,w,w,
                   e,e,e,e,e,e,e,
                     e,e,e,e,e,e,
                       e,e,e,e,e]

      b = Just Black
      w = Just White
      e = Nothing


fresh'grid = hexHexGrid 5
fresh'board = lazyGridMap fresh'grid  []

-- board'indices = indices fresh'grid

-- wszystkie możliwe interpretacje kierunków "Next" oraz "Diag".
ways = aux n0 ++ aux n0'r
    where
      n0 = neighbours (0,0) fresh'grid
      n0'r = reverse n0

      aux xs = zip xs (drop 1 (cycle xs))


--- mapuje każde pole do pojedynczej wartości -- 0, 1 lub 2
appendBoardCSVFile brd handle = do
  let values = map snd $ sort $ GridMap.toList brd
      fieldToDouble Nothing = 0
      fieldToDouble (Just White) = 1
      fieldToDouble (Just Black) = 2
  hPutStr handle (intercalate "," (map (show . fieldToDouble) values))
  hPutStr handle "\n"
  hFlush handle

--- mapuje każde pole do 3 wartości, każda może mieć wartość 0 lub 1.
appendBoardCSVFileSparse brd handle = do
  let values = map snd $ sort $ GridMap.toList brd

      boolToDouble False = 0
      boolToDouble True = 1

      mkVector val = map boolToDouble $ map (==val) values

      vecEmpty = mkVector Nothing
      vecWhite = mkVector (Just White)
      vecBlack = mkVector (Just Black)
      
      vecAll = vecEmpty ++ vecWhite ++ vecBlack

  hPutStr handle (intercalate "," (map show vecAll))
  hPutStr handle "\n"
  hFlush handle

