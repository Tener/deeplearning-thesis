{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-} 

module Board where

import Data.List (sort, intercalate) -- sortBy, nubBy, groupBy, nub, sort, group)
import qualified Data.List.Split as Split
import Data.Maybe (catMaybes)
-- import Data.Ord
import System.IO

import Math.Geometry.Grid

import qualified Math.Geometry.GridMap as GridMap 

import qualified Data.HashMap as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Hashable as Hashable

import qualified Data.Packed.Vector as V

data Color = Black | White deriving (Eq, Ord, Read, Show)

type BoardOld = GridMap.GridMap HexHexGrid Position (Maybe Color)
type Position = (Int,Int) -- fixme: replace with new datatype with hashing function: (\(a,b) -> a*32 + b)
type Direction = (Position, Position)
type BoardHM = HashMap.Map Position Color
data Board = Board { hashmap :: !BoardHM
                   , countWhite :: !Int
                   , countBlack :: !Int
                   } deriving (Show, Eq)

onHashMap :: (BoardHM -> BoardHM) -> Board -> Board
onHashMap f brd = brd { hashmap = f (hashmap brd) } 

marbleCount :: Color -> Board -> Int
marbleCount White brd = countWhite brd
marbleCount Black brd = countBlack brd


-- fixme deal with that
-- #ifndef DOCTESTING
-- instance Hashable.Hashable Position where
--     hashWithSalt _salt (a,b) = a * 32 + b -- hack hack hack
-- #endif

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

data Field = Empty | Death | Ball | Opponent deriving (Eq, Ord, Read, Show)
data Match = And !Match !Match 
           | Or !Match !Match
           | This !Field
           | Next !Match
           | Diag !Match
             deriving (Eq, Ord, Read, Show)

data Setter = SetHere Field | SetNext Setter | SetDiag Setter
             deriving (Eq, Ord, Read, Show)

type Move = [Setter]

-- negacja koloru
negColor :: Color -> Color
negColor Black = White
negColor White = Black

-- | swaps everything on board for other color
-- >>> (negateBoard . negateBoard) starting'board'default == starting'board'default
-- True
negateBoard :: Board -> Board
negateBoard (Board hm cw cb) = Board (HashMap.map negColor hm) cb cw

{-# INLINE rules #-}
rules = [ ("here", ways'straight, here,    move'here) 
        , ("here2", ways'straight, here2,   move'here2) 
        , ("here3", ways'straight, here3,   move'here3) 
        , ("shove2", ways'straight, shove2,  move'shove2) 
        , ("shove31", ways'straight, shove31, move'shove31) 
        , ("shove32", ways'straight, shove32, move'shove32) 
        , ("move'diag2", ways'diag, diag2,   move'diag2) 
        , ("move'diag3", ways'diag, diag3,   move'diag3)]

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
       diag = And (This Ball) (Diag (This Empty)) -- pomocniczy ruch
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

getMovesDir col brd way pos ruleset = [ (name,runMove col brd pos way setter)
                                      | (name,ways,rule,setter) <- ruleset
                                      , HashMap.lookup pos (hashmap brd) == Just col
         
                                      , way'd <- ways
                                      , way'd == way
                                      
                                      , tryMatch col brd pos way rule
                                      ]
getMoves :: Color -> Board -> [Board]
getMoves col brd = -- nub
                   [ runMove col brd idx way setter | 
                     (idx,val) <- HashMap.toList (hashmap brd)
                   , val == col
                   , (_name,ways,rule,setter) <- rules
                   , way <- ways
                   , tryMatch col brd idx way rule
                   ]

getMovesN col brd = 
                   [ (name, way, runMove col brd idx way setter) | 
                     (idx,val) <- HashMap.toList (hashmap brd)
                   , val == col
                   , (name,ways,rule,setter) <- rules
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

updateCounts :: Board -> Maybe Color -> Maybe Color -> Board
updateCounts brd a b | a == b = brd
                     | otherwise = let t2 = b2i $ a == Just White
                                       t3 = b2i $ a == Just Black
                                       t5 = b2i $ b == Just White
                                       t6 = b2i $ b == Just Black 
                                       cw = countWhite brd
                                       cb = countBlack brd
                                       b2i b = if b then 1 else 0
                                   in
                                    brd { countWhite = cw-t2+t5
                                        , countBlack = cb-t3+t6 }
                                       

putBall :: (Position,Color) -> Board -> Board
putBall (pos,color) brd = updateCounts (onHashMap (HashMap.insert pos color) brd) (HashMap.lookup pos (hashmap brd)) (Just color)

runSetter :: Color -> Board -> Position -> (Position, Position) -> Setter -> Board
runSetter side brd pos dir@(dnext, ddiag) setter = 
    case setter of
      (SetHere fld) -> 
          let current = HashMap.lookup pos (hashmap brd) in
          case fld of
                         Ball -> updateCounts (onHashMap (HashMap.insert pos side) brd) current (Just side) -- hack: we *know* that the position is valid
                         Opponent -> if HashSet.member pos gridPositionsHashset 
                                      then updateCounts (onHashMap (HashMap.insert pos (negColor side)) brd) current (Just (negColor side))
                                      else brd -- dont do anything
                         Empty -> updateCounts (onHashMap (HashMap.delete pos) brd) current Nothing 
                         Death -> error "Cant set DEATH anywhere!"
      (SetNext set) -> runSetter side brd (advancePosition pos dnext) dir set
      (SetDiag set) -> runSetter side brd (advancePosition pos ddiag) dir set


gridPositionsHashset :: HashSet.Set Position
gridPositionsHashset = HashSet.fromList (indices fresh'grid)

{-# INLINE advancePosition #-}
advancePosition :: Position -> Position -> Position
advancePosition pos@(f,s) dir@(df, ds) = (f + df, s + ds)

-- wypróbuj ruch
tryMatch :: Color -> Board -> Position -> Direction -> Match -> Bool
tryMatch side brd pos dir@(dnext, ddiag) match = 
    case match of
      And m1 m2 -> {-# SCC tryMatch_and #-} ((tryMatch side brd pos dir m1) && (tryMatch side brd pos dir m2))
      Or m1 m2 -> {-# SCC tryMatch_or #-} ((tryMatch side brd pos dir m1) || (tryMatch side brd pos dir m2))
      Next m -> {-# SCC tryMatch_next #-} tryMatch side brd (advancePosition pos dnext) dir m
      Diag m -> {-# SCC tryMatch_diag #-} tryMatch side brd (advancePosition pos ddiag) dir m
      This f -> {-# SCC tryMatch_this #-} 
                case f of
                  Ball -> HashMap.lookup pos hashmap' == (Just side)
                  Opponent -> HashMap.lookup pos hashmap' == (Just (negColor side))
                  Empty -> (HashSet.member pos gridPositionsHashset) && (HashMap.lookup pos hashmap' == Nothing)
                  Death -> not (HashSet.member pos gridPositionsHashset)
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

-- | konwersja stary -> nowy
-- >>> boardOldToBoard (boardToBoardOld starting'board'default) == starting'board'default
-- True
boardToBoardOld :: Board -> BoardOld
boardToBoardOld brd = foldl (\ old (k,v) -> GridMap.adjust (const v) k old) empty'board (zip keys vals)
    where
      keys = indices fresh'grid
      vals = map (\ k -> HashMap.lookup k (hashmap brd)) keys
      empty'vals = map (const Nothing) keys
      empty'board = GridMap.lazyGridMap fresh'grid empty'vals


-- | przekształca gęstą reprezentację do tablicy
--
-- >>> starting'board'belgianDaisy == (denseReprToBoard $ boardToDense $ starting'board'belgianDaisy)
-- True
denseReprToBoard :: [Int] -> Board
denseReprToBoard xs = boardOldToBoard (GridMap.lazyGridMap fresh'grid balls)
    where
      balls = map intToField xs
      intToField 0 = Nothing
      intToField 1 = (Just White)
      intToField 2 = (Just Black)

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

boardFromBalls :: [Maybe Color] -> Board
boardFromBalls balls = boardOldToBoard $ GridMap.lazyGridMap fresh'grid balls

--- functions for working with textual game notation used by Aba-Pro and others

-- boardFromCoords c'black c'white = undefined
-- b0 = boardFromCoords "i9 h9 g9 h8 g8 f8 h7 g7 f7 g6" "f3 f4 f5 f6 e2 e4 e5 e6 d2 d4 d5 c3 c4"
-- textcoords = concat ["i5 i6 i7 i8 i9             "
--                     ,"h4 h5 h6 h7 h8 h9          "
--                     ,"g3 g4 g5 g6 g7 g8 g9       "
--                     ,"f2 f3 f4 f5 f6 f7 f8 f9    "
--                     ,"e1 e2 e3 e4 e5 e6 e7 e8 e9 "
--                     ,"d2 d3 d4 d5 d6 d7 d8 d9    "
--                     ,"c3 c4 c5 c6 c7 c8 c9       "
--                     ,"b4 b5 b6 b7 b8 b9          "
--                     ,"a5 a6 a7 a8 a9             "
--                     ]


textcoords :: [String]
textcoords = concat $ 
             [g n 'a' l | (n,l) <- zip [1..5] ['e'..]] ++ 
             [g n l 'i' | (n,l) <- zip [6..9] ['b'..]]

    where g n ll lh = reverse $  map (\l -> l:show n) [ll..lh]

lookupTxtCoords :: String -> Maybe (Int, Int)
lookupTxtCoords txt = lookup txt (zip textcoords (indices fresh'grid))

runTxtMove :: Color -> Board -> String -> Board
runTxtMove col brd move = case getMovesDir col brd way pos ruleset of
                            [] -> error $ show ("ERROR: runTxtMove: no moves", move, col, way, pos) 
                                  ++ " " ++ pr brd 
                                  ++ " way=" ++ show way 
                                  ++ " ruleset=" ++ show (map (\(n,_,_,_) -> n) ruleset)
                                  ++ " debug=" ++ debug

                            [(move'name,brd'moved)] -> brd'moved
                            xs -> error $ "runTxtMove: ambiguous move: " ++ move
    where
      pr b = "http://localhost:3000/board/" ++ (reprToRow $ boardToDense $ b)

      (pos,way,ruleset,debug) = case catMaybes $ map lookupTxtCoords $ Split.chunksOf 2 move of
                        [p1,p2] -> (p1,(getVector p1 p2, way'diag'none),rs1,"")
                        [p1,p2,p3] | distanceDiag p1 p2 == 1 -> (p1,(getVector p1 p3, getVector p1 p2), rs2, "")
                                   | distanceDiag p1 p2 == 2 -> (p1,(getVector p1 p3, (div2Pair $ getVector p1 p2)), rs3, "")
                                   | otherwise -> error "runTxtMove??"
                        _ -> error "runTxtMove?"

      distanceDiag p1 p2 = maximum $ map abs $ (pair2Lst $ getVector p1 p2)
      pair2Lst (a,b) = [a,b]
      div2Pair (x,y) = (x`div`2, y`div`2)

      getVector (x0,y0) (x1,y1) = (x1-x0, y1-y0)

      rs1 = take 6 rules
      rs2 = take 1 $ drop 6 rules
      rs3 = take 1 $ drop 7 rules

--- default boards

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

fresh'grid :: HexHexGrid
fresh'grid = hexHexGrid 5

-- wszystkie możliwe interpretacje kierunków "Next" oraz "Diag".
ways'diag = aux n0 ++ aux n0'r
    where
      n0 = neighbours (0,0) fresh'grid
      n0'r = reverse n0

      aux xs = zip xs (drop 1 (cycle xs))

-- brak ruchu po skosie - specjalna wartość
way'diag'none = (0,0)

ways'straight = zip n0 (cycle [way'diag'none]) 
    where
      n0 = neighbours (0,0) fresh'grid


-- | mapuje każde pole do pojedynczej wartości -- 0, 1 lub 2
--
-- >>> boardToDense starting'board'default
-- [0,0,0,2,2,0,0,0,0,2,2,0,0,0,0,2,2,2,1,0,0,0,0,2,2,2,1,1,1,0,0,0,2,2,2,1,1,1,0,0,0,0,2,1,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0]
boardToDense :: Board -> [Int]
boardToDense brd = map fieldToInt values
  where
    values = map snd $ sort $ GridMap.toList (boardToBoardOld brd)

    fieldToInt :: Maybe Color -> Int
    fieldToInt Nothing = 0
    fieldToInt (Just White) = 1
    fieldToInt (Just Black) = 2

-- | mapuje każde pole do 3 liczb, każda może mieć wartość 0 lub 1.
--
-- >>> boardToSparse starting'board'default
-- [1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-- >>> boardToSparse' starting'board'default
-- [1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

boardToSparse :: Board -> [Int]
boardToSparse brd = [ look c p | c <- [Nothing, Just White, Just Black], p <- pos]
    where
      look c p = if HashMap.lookup p (hashmap brd) == c then 1 else 0
      pos = indices fresh'grid

boardToSparseNN :: Board -> V.Vector Double
boardToSparseNN brd = V.fromList [ look c p | c <- [Nothing, Just White, Just Black], p <- pos]
    where
      look c p = if HashMap.lookup p (hashmap brd) == c then 1 else 0
      pos = indices fresh'grid

boardToDenseNN :: Board -> V.Vector Double
boardToDenseNN brd = V.fromList [ look p | p <- pos]
    where
      look p = fieldToD (HashMap.lookup p (hashmap brd))
      pos = indices fresh'grid

      fieldToD :: Maybe Color -> Double
      fieldToD Nothing = 0
      fieldToD (Just White) = 1
      fieldToD (Just Black) = 2


boardToSparse' :: Board -> [Int]
boardToSparse' brd = vecAll
  where
    values = map snd $ sort $ GridMap.toList (boardToBoardOld brd)

    boolToInt :: Bool -> Int
    boolToInt False = 0
    boolToInt True = 1

    mkVector val = map boolToInt $ map (==val) values

    vecEmpty = mkVector Nothing
    vecWhite = mkVector (Just White)
    vecBlack = mkVector (Just Black)
      
    vecAll = vecEmpty ++ vecWhite ++ vecBlack

reprToRow :: (Show a) => [a] -> String
reprToRow repr = intercalate "," (map show repr)

appendBoardCSVFile :: Board -> Handle -> IO ()
appendBoardCSVFile brd handle = do
  hPutStr handle $ reprToRow $ boardToDense brd
  hPutStr handle "\n"
  hFlush handle

appendBoardCSVFileSparse :: Board -> Handle -> IO ()
appendBoardCSVFileSparse brd handle = do
  hPutStr handle $ reprToRow $ boardToSparse brd
  hPutStr handle "\n"
  hFlush handle

