{-# LANGUAGE TypeFamilies, DefaultSignatures, FlexibleContexts #-} 

-- | generic interface for games
module GenericGame where

import Data.ByteString (ByteString)

-- newtype GameM a = GameM { runGameM :: a } deriving Monad

data Player2 = P1 -- ^ player 1
             | P2 -- ^ player 2
               deriving (Eq,Enum,Show,Ord,Read,Bounded)

-- | class for 2 player games
class Game2 a where
    type MoveDesc a :: *
    type GameRepr a :: *
    type GameParams a :: *

    -- | initiate new game
    freshGame :: GameParams a -> a

    -- | get valid moves for a specified player as a list of new states and a move descriptor. if no moves can be made should return empty list. a "passed" move is allowed by including original state in output list and appropriate moveDesc.
    movesDesc :: a -> Player2 -> [(MoveDesc a, a)]
    -- | same as movesDesc but without descriptors. default implementation could be overriden for efficiency.
    moves :: a -> Player2 -> [a]
    moves g p = map snd $ movesDesc g p 

    -- | apply move description to get a new valid state. for invalid moveDesc Nothing shall be returned.
    applyMove :: a -> MoveDesc a -> Maybe a

    -- | returns a winner if the game is finished. can return either of players if both have the winning condition
    winner :: a -> Maybe Player2 

    -- better interface?
    -- | serialize game to bytestring. default implementation uses serializeRepr and toRepr
    serializeGame :: a -> ByteString
    default serializeGame :: (Repr (GameRepr a)) => a -> ByteString
    serializeGame = serializeRepr . toRepr
    -- | deserialize game from bytestring. default implementation uses deserializeRepr and fromRepr
    deserializeGame :: GameParams a -> ByteString -> a
    default deserializeGame :: (Repr (GameRepr a)) => GameParams a -> ByteString -> a
    deserializeGame params = fromRepr params . deserializeRepr

    -- | for neural network training. possibly double too? or boolean? find out.
    toRepr :: a -> GameRepr a
    fromRepr :: GameParams a -> GameRepr a -> a

class Repr gameRepr where
    serializeRepr :: gameRepr -> ByteString
    deserializeRepr :: ByteString -> gameRepr
