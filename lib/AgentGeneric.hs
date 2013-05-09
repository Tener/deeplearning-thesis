{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies, Rank2Types #-}

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
import Text.Printf

import GenericGame
import MinimalNN
import NeuralNets(parseNetFromFile, doubleToEvalInt)

import Data.Packed.Vector as Vector
import qualified Data.Tree.Game_tree.Negascout as GTreeAlgo
import Data.Tree.Game_tree.Game_tree as GTree

-- | agent for Game2 games
class Agent2 a where
    type AgentParams a :: *
    -- | make new agent out of thin air using supplied neural network for evaluation
    mkAgent :: AgentParams a -> IO a
    -- | apply agent decision generating new game state playing as specified player.
    applyAgent :: (Game2 g, Repr (GameRepr g)) => a -> g -> Player2 -> IO g
    -- | get agent name
    agentName :: a -> String

-- | agent that picks a random move from all available moves
data AgentRandom = AgentRandom GenIO

-- | agent that picks a random move from all available *best* moves, where move fitness is determined by neural network evaluation
data AgentSimple = AgentSimple TNetwork GenIO

-- | agent based on monte carlo tree search: evaluate moves counting how many times following that move and playing randomly till the end yields victory.
data AgentMCTS = AgentMCTS Int   -- how many games to evaluate for each possible move
                           AgentRandom -- cached AgentRandom for random game walks
                           GenIO 

-- | the usual alpha-beta etc. based agent.
data AgentGameTree = AgentGameTree TNetwork 
                                   Int      -- search depth

-- | datatype useful for various game-tree-search based agents
data GameStateGen g = GameStateGen { gsgGame :: g
                                   , gsgEvalSelf :: (GameStateGen g) -> Int
                                   , gsgPlayerBase :: Player2 -- ^ player that initialized tree search
                                   , gsgPlayerNow :: Player2  -- ^ player which makes the move now
                                   } 
 
instance (Game2 g) => GTree.Game_tree (GameStateGen g) where
    is_terminal gs = winner (gsgGame gs) /= Nothing
    node_value gs = (gsgEvalSelf gs) gs
    children gs | is_terminal gs = []
                | otherwise = [ GameStateGen g (gsgEvalSelf gs) (gsgPlayerBase gs) (nextPlayer $ gsgPlayerNow gs) 
                                | g <- moves (gsgGame gs) (gsgPlayerNow gs)
                              ]

nextPlayer :: Player2 -> Player2
nextPlayer P1 = P2
nextPlayer P2 = P1

-- | helper class for datatypes containing GenIO inside
class HasGen a where gen :: a -> GenIO
instance HasGen AgentRandom where gen (AgentRandom g) = g
instance HasGen AgentSimple where gen (AgentSimple _ g) = g
instance HasGen AgentMCTS where gen (AgentMCTS _ _ g) = g

class HasTNet a where tnet :: a -> TNetwork
instance HasTNet AgentSimple where tnet (AgentSimple t _) = t
instance HasTNet AgentGameTree where tnet (AgentGameTree t _) = t

data AgentTrace a = AgentTrace IOAct a
data IOAct = IOAct (forall g . IO g -> IO g)
instance (Agent2 a) => Agent2 (AgentTrace a) where
    type AgentParams (AgentTrace a) = (IOAct, AgentParams a)
    mkAgent (trace, params) = AgentTrace trace <$> mkAgent params
    applyAgent (AgentTrace (IOAct trace) agent) g p = trace (applyAgent agent g p)
    agentName (AgentTrace _ a) = agentName a

instance Agent2 AgentRandom where
    type AgentParams AgentRandom = ()
    mkAgent () = AgentRandom <$> (withSystemRandom $ asGenIO $ return)
    agentName _ = "AgentRandom"
    applyAgent agent g p = do
      let mv = moves g p
      when (null mv) (fail "AgentRandom: Stuck, cant do anything.")
      pickList (gen agent) mv

showTn :: TNetwork -> String
showTn tn = show $ (map Vector.dim (biases tn))

instance Agent2 AgentSimple where
    type AgentParams AgentSimple = TNetwork
    mkAgent tn = AgentSimple tn <$> (withSystemRandom $ asGenIO $ return)
    agentName (AgentSimple tn _) = printf "AgentSimple(|tn|=%s)" (showTn tn)
    applyAgent agent g p = do
      let mv = moves g p
          mv'evaled = zip (map (evalGameTNetwork (tnet agent)) mv) mv
          best'moves = map snd $ head $ reverse $ groupBy (\a b -> fst a == fst b) $ sortBy (comparing fst) $ mv'evaled

      when (null best'moves) (fail "AgentSimple: Stuck, no moves left.")
      pickList (gen agent) best'moves

instance Agent2 AgentGameTree where
    type AgentParams AgentGameTree = (TNetwork, Int)
    mkAgent (tn,depth) = return $ AgentGameTree tn depth
    agentName (AgentGameTree tn depth) = printf "AgentGameTree(d=%d, tn=%s)" depth (showTn tn)
    applyAgent agent@(AgentGameTree _ depth) g p = do
      let gs = GameStateGen { gsgGame = g
                            , gsgEvalSelf = eval
                            , gsgPlayerBase = p
                            , gsgPlayerNow = p
                            }

          eval gameSt = fixSign (gsgPlayerNow gameSt) $ doubleToEvalInt $ evalGameTNetwork (tnet agent) $ gsgGame gameSt 
          (princ, _score) = GTreeAlgo.negascout gs depth
          -- fixme: make sure this is correct.
          fixSign P1 v = v
          fixSign P2 v = negate v

      return (gsgGame $ head $ tail $ princ)

mkAgentSimpleFile :: FilePath -> IO AgentSimple
mkAgentSimpleFile fp = do
  (net,_) <- parseNetFromFile `fmap` readFile fp
  mkAgent net

instance Agent2 AgentMCTS where
    type AgentParams AgentMCTS = Int
    mkAgent games = AgentMCTS games <$> mkAgent () <*> (withSystemRandom $ asGenIO $ return)
    agentName (AgentMCTS g _ _) = printf "AgentMCTS(g=%d)" g
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

-- evalGameTNetworkLL :: (Game2 g, Repr (GameRepr g)) => TNetwork -> TNetwork -> g -> Double
-- evalGameTNetworkLL tn1 tn2 g = sumElements $ computeTNetworkSigmoid tn2 $ computeTNetworkSigmoid tn1 $ (reprToNN (toRepr g))

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


