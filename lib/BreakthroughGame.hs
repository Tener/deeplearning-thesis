{-# LANGUAGE ViewPatterns, TypeFamilies, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings #-} 

module BreakthroughGame where

import GenericGame

import qualified Data.HashMap as HashMap
import qualified Data.HashSet as HashSet
import Data.HashMap (Map)
import Data.HashSet (Set)
import Data.Monoid (mempty, mappend)
import Data.Maybe

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BSC8

import qualified Data.Packed.Vector as V

type Position = (Int,Int) -- ^ (x,y), x=column goes from 0 to width-1, y=row goes from 0 to height-1
type BoardMap = Map Position Player2

data Breakthrough = Breakthrough { board :: BoardMap -- ^ map from position to P1 or P2 piece.
                                 , boardSize :: (Int,Int) -- ^ width, height. height >= 4, width >= 2. each player will be given exactly 2*width pieces. there are 'width' many columns and 'height' many rows.
                                 , winningP1 :: Set Position -- ^ fixed set of winning positions for P1
                                 , winningP2 :: Set Position -- ^ fixed set of winning positions for P2
                                 , countP1 :: !Int -- ^ how many pieces P1 have
                                 , countP2 :: !Int -- ^ how many pieces P2 have
                                 } deriving (Show)

instance GameTxtRender Breakthrough where
    prettyPrintGame g = let ll = "---------------------------" 
                            rl = fst (boardSize g)
                            charPos pos = case HashMap.lookup pos (board g) of
                                            Nothing -> '.'
                                            Just P1 -> '1'
                                            Just P2 -> '2'
                            pprow n = "> " ++ map charPos (row n rl) ++ " <"
                        in
                        unlines ( 
                        [ll
                        ,show g
                        ,ll] ++
                        map pprow [0..(snd (boardSize g))-1]
                        ++ [ll])

instance Game2 Breakthrough where
    type MoveDesc Breakthrough = (Position,Position) -- first position, second position
    type GameRepr Breakthrough = [Int] -- sparse field repr.
    type GameParams Breakthrough = (Int,Int) -- board size

    freshGame bsize@(sw,sh) = Breakthrough { board = HashMap.fromList [ (pos,P1) | pos <- row 0 sw ] `mappend`
                                                     HashMap.fromList [ (pos,P1) | pos <- row 1 sw ] `mappend`
                                                     HashMap.fromList [ (pos,P2) | pos <- row (sh-2) sw ] `mappend`
                                                     HashMap.fromList [ (pos,P2) | pos <- row (sh-1) sw ]
--                                             HashMap.fromList [ (pos,pl) | (pl,rows) <- [(P1,[0,1]), (P2, [(sh-1,sh-2)])]
--                                                                           , row'num <- rows
--                                                                           , pos <- row row'num sw ]
                                           , boardSize = bsize
                                           , winningP1 = HashSet.fromList (row (sh-1) sw)
                                           , winningP2 = HashSet.fromList (row 0 sw)
                                           , countP1 = 2 * sw
                                           , countP2 = 2 * sw
                                           }
    
    winner g | countP1 g == 0 = return P2
             | countP2 g == 0 = return P1
             | (getAllSet P1 (board g) `HashSet.intersection` winningP1 g) /= mempty = return P1
             | (getAllSet P2 (board g) `HashSet.intersection` winningP2 g) /= mempty = return P2
             | otherwise = fail "no winners"

    movesDesc g p = let dirs = case p of
                                  P1 -> [(1,1),(0,1),(-1,1)] -- move upward
                                  P2 -> [(1,-1),(0,-1),(-1,-1)] -- move downward
                        brd = board g
                        applyDir (x,y) (dx,dy) = (x+dx, y+dy)
                        movesPos = [ (pos1, applyDir pos1 dir) | pos1 <- getAll p brd, dir <- dirs]
                        boardsPos = map (applyMove g) movesPos

                        uplift :: (a, Maybe b) -> Maybe (a,b)
                        uplift (x,my) = do
                           y <- my
                           return (x,y)
                    in
                        catMaybes $ map uplift $ zip movesPos boardsPos

    -- uses default implementation for 'moves':
    -- moves g p = map snd $ movesDesc g p 

    applyMove g (p1,p2)
       | p1 `illegalPos` g = Nothing
       | p2 `illegalPos` g = Nothing
       | otherwise = let b0 = board g
                         b1 = HashMap.alter (const $ valPos1) p2 b0
                         b2 = HashMap.delete p1 b1
                         valPos1 = HashMap.lookup p1 b0
                         valPos2 = HashMap.lookup p2 b0

                         -- board after move
                         brdOK = g { board = b2 }

                         -- move removing P1 piece
                         brdOK'P2P1 = brdOK { countP1 = (countP1 g) - 1 }

                         -- move removing P2 piece
                         brdOK'P1P2 = brdOK { countP2 = (countP2 g) - 1 }

                         isDiagonalMove = (fst p1 - fst p2) /= 0

                     in
                       case (valPos1, valPos2) of
                         (Just P1,Just P2) | isDiagonalMove -> return brdOK'P1P2
                                           | otherwise -> Nothing -- we can only kill moving diagonally
                         (Just P2,Just P1) | isDiagonalMove -> return brdOK'P2P1
                                           | otherwise -> Nothing -- we can only kill moving diagonally
                         (Just _,Nothing) -> return brdOK
                         (Nothing,_) -> Nothing -- we cannot move non-existing piece
                         (Just P1,Just P1) -> Nothing -- we cannot kill our own pieces
                         (Just P2,Just P2) -> Nothing -- we cannot kill our own pieces

    toRepr g = let 
        pos = allPos (boardSize g)
        look c p = if HashMap.lookup p (board g) == c then one else zero
        repr = [ look c p | c <- [Nothing, Just P1, Just P2], p <- pos]
     in repr
    fromRepr params repr = let
        g0 :: Breakthrough
        g0 = freshGame params
        pos :: (Maybe Player2) -> [((Maybe Player2),Position)]
        pos c = zip (cycle [c]) (allPos (boardSize g0))
        cs :: [Maybe Player2]
        cs = [Nothing, Just P1, Just P2]

        pos3 :: [((Maybe Player2),Position)]
        pos3 = concatMap pos cs

        b0 = board g0 
        b1 = foldl update b0 (zip pos3 repr) 
            where update b ((c,po),val) | val == zero = b
                                        | val == one = HashMap.alter (const c) po b
                                        | otherwise = error "fromRepr: bad val"

        g1 = g0 { board = b1
                , countP1 = count P1 b1
                , countP2 = count P2 b1
                }
     in g1

class OneZero a where
    zero, one :: a
    default zero :: (Enum a) => a
    zero = toEnum 0
    default one :: (Enum a) => a
    one = toEnum 1

instance OneZero Int where
instance OneZero Bool where
instance OneZero Double where

instance Repr [Int] where
    serializeRepr repr = ByteString.intercalate "," (map (BSC8.pack . show) repr)
    deserializeRepr = error "deserializeRepr :: [Int] -> ByteString : not implemented" -- fixme
    reprToNN repr = V.fromList (map fromIntegral repr)

instance Repr [Double] where
    serializeRepr repr = ByteString.intercalate "," (map (BSC8.pack . show) repr)
    deserializeRepr = error "deserializeRepr :: [Double] -> ByteString : not implemented" -- fixme
    reprToNN repr = V.fromList repr

illegalPos :: Position -> Breakthrough -> Bool
illegalPos _pos@(x,y) g 
    | y < 0 = True
    | y >= (snd (boardSize g)) = True
    | x < 0 = True
    | x >= (fst (boardSize g)) = True
    | otherwise = False

legalPos :: Position -> Breakthrough -> Bool
legalPos pos brd = not (illegalPos pos brd)

-- | get all positions within a specified row. 
row rowNum rowLength = [(column',rowNum) | column' <- [0..rowLength-1]]
{-# INLINE row #-}

-- | get all positions within a specified column. 
column colNum colLength = [(colNum,row') | row' <- [0..colLength-1]]
{-# INLINE column #-}

-- | get all possible positions
allPos (rowLength,colLength) = [(column',row') | row' <- [0..colLength-1], column' <- [0..rowLength-1]]
{-# INLINE allPos #-}

-- | get all positions of pieces of specified player on board (as a set)
getAllSet :: Player2 -> BoardMap -> Set Position
getAllSet el hm = HashSet.fromList $ HashMap.keys $ HashMap.filter (==el) hm

-- | get all positions of pieces of specified player on board (as a list)
getAll :: Player2 -> BoardMap -> [Position]
getAll el hm = HashMap.keys $ HashMap.filter (==el) hm

-- | count number of pieces of specified player on board
count :: Player2 -> BoardMap -> Int
count p hm = length $ filter (==p) $ map snd $ HashMap.toList hm

-- debugging utils 
pp g = putStrLn . prettyPrintGame $ g

g0 :: Breakthrough
g0 = freshGame (6,6)

