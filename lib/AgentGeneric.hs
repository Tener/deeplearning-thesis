{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

-- | few agents for generic games as defined in 'GenericGame'
module AgentGeneric where

import System.Random.MWC
import Control.Applicative
import Control.Monad (when)
import Numeric.Container (sumElements)
import Data.Default

import GenericGame
import MinimalNN


-- | agent for Game2 games
class Agent2 a where
    type AgentParams a :: *
    -- | make new agent out of thin air using supplied neural network for evaluation
    mkAgent :: AgentParams a -> IO a
    -- | apply agent decision generating new game state playing as specified player.
    applyAgent :: (Game2 g, Repr (GameRepr g)) => a -> g -> Player2 -> IO g

-- | agent that picks a random move from all available moves
data AgentRandom = AgentRandom GenIO

-- | agent that picks a random move from all available *best* moves, where move fitness is determined by neural network evaluation
data AgentSimple = AgentSimple TNetwork GenIO

-- | agent based on monte carlo tree search: evaluate moves counting how many times following that move and playing randomly till the end yields victory.
data AgentMCTS = AgentMCTS GenIO

-- import qualified Data.Tree.Game_tree.Negascout as GTreeAlgo
-- import Data.Tree.Game_tree.Game_tree as GTree
-- data AgentGameTree = AgentGameTree    deriving (Eq, Ord, Show)
-- -- | datatype useful for various game-tree-search based agents
-- data GameStateGen g = GameStateGen g { gsgGame :: g
--                                      , gsgEvalSelf :: GameState -> Int
--                                      , gsgPlayerBase :: Player2 -- ^ player that initialized tree search
--                                      , gsgPlayerNow :: Player2  -- ^ player which makes the move now
--                                      } 
--  
-- instance GTree.Game_tree GameState where
--     is_terminal gst = Board.isFinished (unwrap $ gtBoard gst)
--     node_value gst = (gtEvalSelf gst) gst
--     children (GameState (BBoard brd0) eval colB colN) | isFinished brd0 = []
--                                                       | otherwise = [ GameState (mkBBoard colN brd) eval colB (Board.negColor $ colN) | brd <- Board.getMoves colN brd0 ]

-- instance Agent2 AgentGameTree where
--     mkAgent = return AgentGameTree
--  
--     applyAgent agent g p = do
--       mv <- moves g p
--       case mv of
--         [] -> do
--           fail "AgentRandom: Stuck, cant do anything."
--         _ -> do
--           pick <- uniformR (0, length mv - 1) (gen agent)
--           let chosen = (mv !! pick)
--           return chosen

-- | helper class for datatypes containing GenIO inside
class HasGen a where gen :: a -> GenIO
instance HasGen AgentRandom where gen (AgentRandom g) = g
instance HasGen AgentSimple where gen (AgentSimple _ g) = g

class HasTNet a where tnet :: a -> TNetwork
instance HasTNet AgentSimple where tnet (AgentSimple t _) = t

instance Agent2 AgentRandom where
    type AgentParams AgentRandom = ()
    mkAgent () = AgentRandom <$> (withSystemRandom $ asGenIO $ return)

    applyAgent agent g p = do
      let mv = moves g p
      when (null mv) (fail "AgentRandom: Stuck, cant do anything.")
      pickList (gen agent) mv

instance Agent2 AgentSimple where
    type AgentParams AgentSimple = TNetwork
    mkAgent tn = AgentSimple tn <$> (withSystemRandom $ asGenIO $ return)

    applyAgent agent g p = do
      let mv = moves g p
          mv'evaled = zip (map (evalGameTNetwork (tnet agent)) mv) mv
          best'moves = groupBy (\a b -> fst a == fst b) $ sort $ mv'evaled

      when (null best'moves) (fail "AgentSimple: Stuck, no moves left.")
      pickList (gen agent) best'moves

pickList rgen xs = do
          pick <- uniformR (0, xs - 1) rgen
          return (xs !! pickxs)

evalGameTNetwork :: (Game2 g, Repr (GameRepr g)) => TNetwork -> g -> Double
evalGameTNetwork tn g = sumElements $ computeTNetworkSigmoid tn (reprToNN (toRepr g))

-- | data structure with callbacks for any occasion
data GameDriverCallback g = GameDriverCallback 
    { -- | game finished possibly with a draw or we run out of time
      gameFinished :: (g -> IO ()) 
      -- | new turn, player which takes move in it. return value True means "continue with game"
    , gameTurn :: (g -> Player2 -> IO Bool) 
    }

instance (Game2 g, Show g) => Default (GameDriverCallback g) where
    def = GameDriverCallback (\g -> putStrLn $ "Game finished! Winner: " ++ show (winner g))
                             (\g p -> return True)

-- | a 'driver' for Game2 games. passed appropriate callback structure drives the game from given state to the end.
driverG2 :: (Game2 g, Repr (GameRepr g), Agent2 a1, Agent2 a2) => g -> a1 -> a2 -> (GameDriverCallback g) -> IO ()
driverG2 g0 a1 a2 cb = do 
  let allSteps = cycle [((applyAgent a1),P1),((applyAgent a2),P2)]
      loop g (_step@(ag,pl):steps) = do
        case winner g of
          Nothing -> do
            cont <- gameTurn cb g pl
            when cont (do
                        g' <- ag g pl
                        loop g' steps)
          Just _p  -> gameFinished cb g

  loop g0 allSteps
