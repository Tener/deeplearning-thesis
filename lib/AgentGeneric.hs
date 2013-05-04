{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

-- | few agents for generic games as defined in 'GenericGame'
module AgentGeneric where

import System.Random.MWC
import Control.Applicative
import Control.Monad (when, replicateM)
import Numeric.Container (sumElements)
import Data.Default
import Data.List (groupBy, sortBy)
import Data.IORef
import Data.Ord

import GenericGame
import MinimalNN
import NeuralNets(parseNetFromFile)

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

-- | same as agent simple but with two networks. 
--   fixme: merge with AgentSimple by merging the networks
data AgentSimpleLL = AgentSimpleLL TNetwork TNetwork GenIO

-- | agent based on monte carlo tree search: evaluate moves counting how many times following that move and playing randomly till the end yields victory.
data AgentMCTS = AgentMCTS Int   -- how many games to evaluate for each possible move
                           AgentRandom -- cached AgentRandom for random game walks
                           GenIO 

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
instance HasGen AgentSimpleLL where gen (AgentSimpleLL _ _ g) = g
instance HasGen AgentMCTS where gen (AgentMCTS _ _ g) = g

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
          best'moves = map snd $ head $ reverse $ groupBy (\a b -> fst a == fst b) $ sortBy (comparing fst) $ mv'evaled

      when (null best'moves) (fail "AgentSimple: Stuck, no moves left.")
      pickList (gen agent) best'moves

instance Agent2 AgentSimpleLL where
    type AgentParams AgentSimpleLL = (TNetwork, TNetwork)
    mkAgent (tn1,tn2) = AgentSimpleLL tn1 tn2 <$> (withSystemRandom $ asGenIO $ return)

    applyAgent agent@(AgentSimpleLL tn1 tn2 _) g p = do
      let mv = moves g p
          mv'evaled = zip (map (evalGameTNetworkLL tn1 tn2) mv) mv
          best'moves = map snd $ head $ groupBy (\a b -> fst a == fst b) $ sortBy (comparing fst) $ mv'evaled

      when (null best'moves) (fail "AgentSimple: Stuck, no moves left.")
      pickList (gen agent) best'moves


mkAgentSimpleFile :: FilePath -> IO AgentSimple
mkAgentSimpleFile fp = do
  (net,_) <- parseNetFromFile `fmap` readFile fp
  mkAgent net

instance Agent2 AgentMCTS where
    type AgentParams AgentMCTS = Int
    mkAgent games = AgentMCTS games <$> mkAgent () <*> (withSystemRandom $ asGenIO $ return)

    applyAgent (AgentMCTS games agRnd _) g p = do
      let mv = moves g p
          myeval move = do
            winners <- replicateM games (winner `fmap` randomGame move)
            let count = length $ filter (==(Just p)) winners
            return (count, move)
          randomGame game = driverG2 game agRnd agRnd (GameDriverCallback (\_ -> return ()) (\_ _ -> return True))
          takeBest n some'moves = take n $ reverse $ map snd $ sortBy (comparing fst) $ some'moves
      
      mv'evaled <- takeBest 3 `fmap` mapM myeval mv
      best'moves <- takeBest 3 `fmap` mapM myeval mv'evaled

      when (null best'moves) (fail "AgentMCTS: Stuck, no moves left.")
      return (head best'moves)

pickList :: GenIO -> [a] -> IO a
pickList rgen xs = do
          pick <- uniformR (0, (length xs) - 1) rgen
          return (xs !! pick)

evalGameTNetwork :: (Game2 g, Repr (GameRepr g)) => TNetwork -> g -> Double
evalGameTNetwork tn g = sumElements $ computeTNetworkSigmoid tn (reprToNN (toRepr g))

evalGameTNetworkLL :: (Game2 g, Repr (GameRepr g)) => TNetwork -> TNetwork -> g -> Double
evalGameTNetworkLL tn1 tn2 g = sumElements $ computeTNetworkSigmoid tn2 $ computeTNetworkSigmoid tn1 $ (reprToNN (toRepr g))

-- | data structure with callbacks for any occasion
data GameDriverCallback g = GameDriverCallback 
    { -- | game finished possibly with a draw or we run out of time
      gameFinished :: (g -> IO ()) 
      -- | new turn, player which takes move in it. return value True means "continue with game"
    , gameTurn :: (g -> Player2 -> IO Bool) 
    }

instance (Game2 g, Show g) => Default (GameDriverCallback g) where
    def = GameDriverCallback (\g -> putStrLn $ "Game finished! Winner: " ++ show (winner g))
                             (\_ _ -> return True)

-- | a 'driver' for Game2 games. passed appropriate callback structure drives the game from given state to the end.
driverG2 :: (Game2 g, Repr (GameRepr g), Agent2 a1, Agent2 a2) => g -> a1 -> a2 -> (GameDriverCallback g) -> IO g
driverG2 g0 a1 a2 cb = do 
  let allSteps = cycle [((applyAgent a1),P1),((applyAgent a2),P2)]
      end g = gameFinished cb g >> return g

      loop _ [] = error "empty cycle?! for real: fix -Wall -Werror"
      loop g (_step@(ag,pl):steps) = do
        case winner g of
          Nothing -> do
            cont <- gameTurn cb g pl
            if cont then (do
                           g' <- ag g pl
                           loop g' steps)
                    else end g
          Just _p  -> end g
  loop g0 allSteps

--------------- various utility functions built on top of driverG2

-- | generate an infinite stream of random games, extracted via callback. exits when provided action returns False.
sampleRandomGames :: (Repr (GameRepr g), Game2 g) 
                  => (IO Bool) -- ^ should we exit now?
                  -> Float -- ^ probability to call a callback on any generated game state
                  -> (g -> IO ()) -- ^ callback
                  -> IO ()
sampleRandomGames canContinue prob cbChosen = do
  mygen <- withSystemRandom $ asGenIO $ return  
  agRnd <- mkAgent () :: IO AgentRandom

  let cb = GameDriverCallback (callbackOut)
                              (\g _ -> callbackOut g >> canContinue)
      callbackOut g = do
        chance <- uniform mygen
        when (chance < prob) (cbChosen g)

  while canContinue (driverG2 freshGameDefaultParams agRnd agRnd cb >> return ())

-- | generate an stream of random games, extracted via callback. exits after calling callback @count@ times.
sampleRandomGamesCount :: (Repr (GameRepr g), Game2 g)
                       => Int -- ^ callback call @count@
                       -> Float -- ^ probability to call a callback on any generated game state
                       -> (g -> IO ()) -- ^ callback
                       -> IO ()
sampleRandomGamesCount count prob cbChosen = do
  countRef <- newIORef 0
  let canContinue = (<count) `fmap` readIORef countRef
      incCount :: IO ()
      incCount = atomicModifyIORef countRef (\ !val -> (val+1,val)) >> return ()
  sampleRandomGames canContinue prob (\g -> incCount >> cbChosen g)

-- | generate an infinite stream depth of random games, extracted via callback. exits when provided action returns False.
sampleRandomGameDepth :: (Repr (GameRepr g), Game2 g) 
                         => (IO Bool) -- ^ should we exit now?
                         -> (g -> Int -> IO ()) -- ^ callback
                         -> IO ()
sampleRandomGameDepth canContinue cbFinished = do
  agRnd <- mkAgent () :: IO AgentRandom
  sampleGameDepth agRnd agRnd canContinue cbFinished 

-- | generate a stream of games between given agents, extracted via callback. exits after calling callback given number of times.
sampleGameDepthCount :: (Repr (GameRepr g), Game2 g, Agent2 a1, Agent2 a2) 
                         => a1
                         -> a2
                         -> Int -- ^ callback call count
                         -> (g -> Int -> IO ()) -- ^ callback
                         -> IO ()
sampleGameDepthCount ag1 ag2 count cbFinished = do
  countRef <- newIORef 0
  let canContinue = (<count) `fmap` readIORef countRef
      incCount :: IO ()
      incCount = atomicModifyIORef countRef (\ !val -> (val+1,val)) >> return ()
  sampleGameDepth ag1 ag2 canContinue (\g i -> incCount >> cbFinished g i)

-- | generate an infinite stream depth of games between given agents, extracted via callback. exits when provided action returns False.
sampleGameDepth :: (Repr (GameRepr g), Game2 g, Agent2 a1, Agent2 a2) 
                         => a1
                         -> a2
                         -> (IO Bool) -- ^ should we exit now?
                         -> (g -> Int -> IO ()) -- ^ callback
                         -> IO ()
sampleGameDepth ag1 ag2 canContinue cbFinished = do
  ref <- newIORef 0
  let cb = GameDriverCallback (\_ -> return ()) (\_ _ -> do
                                                   modifyIORef ref (+1) 
                                                   return True)

  while canContinue (do
            writeIORef ref 0
            done <- driverG2 freshGameDefaultParams ag1 ag2 cb
            !count <- readIORef ref
            cbFinished done count
          )

while :: (Monad m) => (m Bool) -> (m ()) -> (m ())
while go a = do
  b <- go 
  when b (a >> while go a)


