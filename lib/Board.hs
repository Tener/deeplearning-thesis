{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-} 

module Board where

import Prelude hiding (lookup)

import Data.List (sortBy, nubBy, groupBy, nub, sort, group, intercalate)
import Data.Ord
import System.IO

import Math.Geometry.Grid

import qualified Math.Geometry.GridMap as GridMap 

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

type BoardOld = GridMap.GridMap HexHexGrid Position (Maybe Color)
type Position = (Int,Int)
type Direction = (Position, Position)
type BoardHM = HashMap.HashMap Position Color
data Board = Board { hashmap :: !BoardHM
                   , countWhite :: !Int
                   , countBlack :: !Int
                   } deriving Show

onHashMap :: (BoardHM -> BoardHM) -> Board -> Board
onHashMap f brd = brd { hashmap = f (hashmap brd) } 

-- TODO:
-- countAll :: Board' -> Int
-- countEmpty :: Board' -> Int

marbleCount White brd = countWhite brd
marbleCount Black brd = countBlack brd


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
rules = [ (ways'straight, here,    move'here) 
        , (ways'straight, here2,   move'here2) 
        , (ways'straight, here3,   move'here3) 
        , (ways'straight, shove2,  move'shove2) 
        , (ways'straight, shove31, move'shove31) 
        , (ways'straight, shove32, move'shove32) 
        , (ways'diag, diag,    move'diag) 
        , (ways'diag, diag2,   move'diag2) 
        , (ways'diag, diag3,   move'diag3)]

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

-- nubOrd xs = map head $ group $ sort xs
-- nubBoards brds = map (snd . head) $ groupBy cmpFst $ sortBy (comparing fst) $ map wrap $ brds
--     where
--       cmpFst (a,_) (b,_) = a == b
--       cast brd = sort $ GridMap.toList brd
--       wrap brd = (cast brd, brd)

-- getMoves col brd = -- nubBoards
--                    [ runMove col brd idx way setter | 
--                      (rule,setter) <- rules
--                    , (idx,val) <- toList brd
--                    , val == Just col
--                    , way <- ways
--                    , tryMatch col brd idx way rule
--                    ]

getMoves col brd = [ runMove col brd idx way setter | 
                     (ways,rule,setter) <- rules
                   , (idx,val) <- HashMap.toList (hashmap brd)
                   , val == col
                   , way <- ways
                   , tryMatch col brd idx way rule
                   ]

isFinished :: Board -> Bool
isFinished brd = getWinner brd /= Nothing

getWinner :: Board -> Maybe Color
getWinner brd = case () of
                  _ | countWhite brd <= 14-6 -> Just Black
                    | countBlack brd <= 14-6 -> Just White
                    | otherwise -> Nothing

-- wykonaj ruch
runMove :: Color -> Board -> Position -> Direction -> Move -> Board
runMove col brd pos dir move = foldl (\ board setter -> runSetter col board pos dir setter) brd move

runSetter side brd pos dir@(dnext, ddiag) setter = 
    case setter of
      (SetHere fld) -> case fld of
                         Ball -> onHashMap (HashMap.insert pos side) brd 
                         Opponent -> onHashMap (HashMap.insert pos (negColor side)) brd
                         Empty -> onHashMap (HashMap.delete pos) brd
                         Death -> error "Cant set DEATH anywhere!"
      (SetNext set) -> runSetter side brd (advancePosition pos dnext) dir set
      (SetDiag set) -> runSetter side brd (advancePosition pos ddiag) dir set

-- wypróbuj ruch

gridPositionsHashset :: HashSet.HashSet Position
gridPositionsHashset = HashSet.fromList (indices fresh'grid)

advancePosition :: Position -> Position -> Position
advancePosition pos dir = (fst pos + fst dir, snd pos + snd dir)

tryMatch :: Color -> Board -> Position -> Direction -> Match -> Bool
tryMatch side brd pos dir@(dnext, ddiag) match = 
    case match of
      And m1 m2 -> (tryMatch side brd pos dir m1) && (tryMatch side brd pos dir m2)
      Or m1 m2 -> (tryMatch side brd pos dir m1) || (tryMatch side brd pos dir m2)
      Next m -> tryMatch side brd (advancePosition pos dnext) dir m
      Diag m -> tryMatch side brd (advancePosition pos ddiag) dir m
      This f -> case f of
                  Ball -> HashMap.lookup pos hashmap' == (Just side)
                  Opponent -> HashMap.lookup pos hashmap' == (Just (negColor side))
                  Empty -> HashMap.lookup pos hashmap' == Nothing
                  Death -> HashSet.member pos gridPositionsHashset
                where
                  hashmap' = hashmap brd


-- konwersja: stary i nowy typ
boardOldToBoard :: BoardOld -> Board
boardOldToBoard brd = Board newhashmap cnt'w cnt'b
            where
              newhashmap :: BoardHM
              newhashmap = HashMap.fromList (map (\ (k,Just v) -> (k,v)) $ filter ((/=Nothing) . snd) $ GridMap.toList brd)
              cnt'w = length $ filter (==(Just White)) (GridMap.elems brd)
              cnt'b = length $ filter (==(Just Black)) (GridMap.elems brd)

boardToBoardOld :: Board -> BoardOld
boardToBoardOld brd = foldl (\ brd' (k,v) -> GridMap.adjust (const (Just v)) k brd') (GridMap.lazyGridMap fresh'grid []) (HashMap.toList (hashmap brd))

-- http://en.wikipedia.org/wiki/File:Abalone_standard.svg
starting'board'default :: Board
starting'board'default = boardOldToBoard $ GridMap.lazyGridMap fresh'grid balls
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
starting'board'belgianDaisy = boardOldToBoard $ GridMap.lazyGridMap fresh'grid balls
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
starting'board'death = boardOldToBoard $ GridMap.lazyGridMap fresh'grid balls
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

-- fresh'board = GridMap.lazyGridMap fresh'grid  []
-- board'indices = indices fresh'grid

-- wszystkie możliwe interpretacje kierunków "Next" oraz "Diag".
ways'diag = aux n0 ++ aux n0'r
    where
      n0 = neighbours (0,0) fresh'grid
      n0'r = reverse n0

      aux xs = zip xs (drop 1 (cycle xs))

ways'straight = zip n0 (cycle [(0,0)]) 
    where
      n0 = neighbours (0,0) fresh'grid


--- mapuje każde pole do pojedynczej wartości -- 0, 1 lub 2
appendBoardCSVFile brd handle = do
  let values = map snd $ sort $ GridMap.toList (boardToBoardOld brd)
      fieldToInt :: Maybe Color -> Int
      fieldToInt Nothing = 0
      fieldToInt (Just White) = 1
      fieldToInt (Just Black) = 2
  hPutStr handle (intercalate "," (map (show . fieldToInt) values))
  hPutStr handle "\n"
  hFlush handle

--- mapuje każde pole do 3 liczb, każda może mieć wartość 0 lub 1.
appendBoardCSVFileSparse brd handle = do
  let values = map snd $ sort $ GridMap.toList (boardToBoardOld brd)

      boolToInt :: Bool -> Int
      boolToInt False = 0
      boolToInt True = 1

      mkVector val = map boolToInt $ map (==val) values

      vecEmpty = mkVector Nothing
      vecWhite = mkVector (Just White)
      vecBlack = mkVector (Just Black)
      
      vecAll = vecEmpty ++ vecWhite ++ vecBlack

  hPutStr handle (intercalate "," (map show vecAll))
  hPutStr handle "\n"
  hFlush handle

