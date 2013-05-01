{-# LANGUAGE TypeFamilies, DefaultSignatures, FlexibleContexts, OverloadedStrings, FlexibleInstances #-} 

-- | generic interface for games
module GenericGame where

import Data.ByteString (ByteString)
import qualified Data.Packed.Vector as V
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BSC8
import Control.Monad

import MinimalNN

data Player2 = P1 -- ^ player 1
             | P2 -- ^ player 2
               deriving (Eq,Enum,Show,Ord,Read,Bounded)

class GameTxtRender g where
    prettyPrintGame :: g -> String

-- | class for 2 player games
class Game2 a where
    -- | type for move descriptions
    type MoveDesc a :: *
    -- | type for game state representations
    type GameRepr a :: *
    -- | type for game parameters (board size etc.)
    type GameParams a :: *

    -- | initiate new game
    freshGame :: GameParams a -> a

    -- | initiate new game using default parameters
    freshGameDefaultParams :: a

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

    -- | for neural network training. does not have to preserve game parameters.
    toRepr :: a -> GameRepr a
    -- | load from game representation and game parameters
    fromRepr :: GameParams a -> GameRepr a -> a

-- | representing games states
class Repr gameRepr where
    -- | serialize representation to bytestring
    serializeRepr :: gameRepr -> ByteString

    -- | deserialize representation from bytestring
    deserializeRepr :: ByteString -> gameRepr

    -- | transform representation into vector to be fed into NN (for NN evaluation of game state)
    reprToNN :: gameRepr -> V.Vector Double

-- 
-- game2agent :: gameState -> neural network -> best choice gameState
-- game2driver :: gameParams -> agent1 -> agent2 -> stream gameState
-- game2learner :: [gameState] -> neural network evaluator??
-- game2vis :: gameState -> svg diagram

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


  
