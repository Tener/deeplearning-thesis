{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies, Rank2Types #-}

-- | few agents for generic games as defined in 'GenericGame'
module AgentGeneric where

import System.Random.MWC
import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Monad (when, replicateM)
--import Numeric.Container (sumElements)
import Data.Default
import Data.List (groupBy, sortBy)
import Data.IORef
import Data.Ord
import Text.Printf

import GenericGame
import MinimalNN
import ThreadLocal
import qualified MyVectorType as V
import NeuralNets(parseNetFromFile, doubleToEvalInt)

import Data.Chronograph hiding (val)

import qualified Data.Tree.Game_tree.Negascout as GTreeAlgo
import Data.Tree.Game_tree.Game_tree as GTree

-- | agent for Game2 games
class Agent2 a where
    type AgentParams a :: *
    -- | make new agent out of thin air using supplied neural network for evaluation
    mkAgent :: AgentParams a -> ThrLocIO a
    -- | apply agent decision generating new game state playing as specified player.
    applyAgent :: (Game2 g, Repr (GameRepr g)) => a -> g -> Player2 -> ThrLocIO g
    -- | get agent name
    agentName :: a -> String

class Agent2Eval a where
    evaluateGame :: (Game2 g, Repr (GameRepr g)) => a -> Player2 -> g -> Double

-- | summing agent, adds evaluations of any players it holds
data AgEvalPlus a1 a2 = AgEvalPlus a1 a2
instance (Agent2Eval a1, Agent2Eval a2) => Agent2Eval (AgEvalPlus a1 a2) where
    evaluateGame (AgEvalPlus a1 a2) pl g = (evaluateGame a1 pl g) + (evaluateGame a2 pl g)

-- | constant player, everything is the same
data AgEvalConst = AgEvalConst Double
instance Agent2Eval AgEvalConst where evaluateGame (AgEvalConst val) _ _ = val

-- | agent that picks a random move from all available moves
data AgentRandom = AgentRandom GenIO

-- | agent that picks a weighted random move from all available moves. weights are provided by game evaluator agent and ammended by bias
data AgentRandomSkew agEval = AgentRandomSkew Double -- bias, value added to each evaluation performed by agEval 
                                              agEval -- game evaluator
                                              GenIO  -- cached GenIO

-- | agent that picks a random move from all available *best* moves, where move fitness is determined by neural network evaluation
data AgentSimple nn = AgentSimple nn GenIO

-- | agent based on monte carlo tree search: evaluate moves counting how many times following that move and playing randomly till the end yields victory.
data AgentMCTS = AgentMCTS Int   -- how many games to evaluate for each possible move
                           AgentRandom -- cached AgentRandom for random game walks
                           GenIO 

-- | agent based on monte carlo tree search: evaluate moves counting how many times following that move and playing randomly till the end yields victory.
data AgentParMCTS ag = AgentParMCTS Double -- bias, see AgentRandomSkew
                                    Int    -- how many games to evaluate for each possible move
                                    ag     -- helper agent to evaluate moves
                                    GenIO 

-- | the usual alpha-beta etc. based agent.
data AgentGameTree nn = AgentGameTree nn 
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

-- | helper class for datatypes containing GenIO inside
class HasGen a where gen :: a -> GenIO
instance HasGen AgentRandom where gen (AgentRandom g) = g
instance HasGen (AgentSimple nn) where gen (AgentSimple _ g) = g
instance HasGen AgentMCTS where gen (AgentMCTS _ _ g) = g

data AgentTrace a = AgentTrace IOAct a
data IOAct = IOAct (forall g . IO g -> IO g)
instance (Agent2 a) => Agent2 (AgentTrace a) where
    type AgentParams (AgentTrace a) = (IOAct, AgentParams a)
    mkAgent (trace, params) = AgentTrace trace <$> mkAgent params
    applyAgent (AgentTrace (IOAct trace) agent) g p = trace (applyAgent agent g p)
    agentName (AgentTrace _ a) = agentName a

mkTimed :: (Agent2 a, AgentParams a ~ (IOAct, arg)) => String -> arg -> ThrLocIO a
mkTimed label arg = mkAgent ((IOAct (timed label)), arg)

timed :: (Show t) => t -> IO b -> ThrLocIO b
timed s a = do
  (Chronograph r t) <- chronoIO a
  printTL (s,t)
  return r


instance Agent2 AgentRandom where
    type AgentParams AgentRandom = ()
    mkAgent () = AgentRandom <$> (withSystemRandom $ asGenIO $ return)
    agentName _ = "AgentRandom"
    applyAgent agent g p = do
      let mv = moves g p
      when (null mv) (fail "AgentRandom: Stuck, cant do anything.")
      pickList (gen agent) mv

instance Agent2Eval AgentRandom where
    evaluateGame _ _ _ = 1

instance (Agent2Eval agEval) => Agent2 (AgentRandomSkew agEval) where
    type AgentParams (AgentRandomSkew agEval) = (Double, agEval, GenIO)
    mkAgent (bias,agEval,rgen) = return (AgentRandomSkew bias agEval rgen)
    agentName _ = "AgentRandomSkew"
    applyAgent (AgentRandomSkew bias agEval rgen) g p = do
      let mv = map ((\g' -> bias + evaluateGame agEval p g') &&& id) (moves g p)
      when (null mv) (fail "AgentRandomSkew: Stuck, cant do anything.")      
      pickListWeighted rgen mv

instance (NeuralNetwork nn) => Agent2 (AgentSimple nn) where
    type AgentParams (AgentSimple nn) = nn
    mkAgent tn = AgentSimple tn <$> (withSystemRandom $ asGenIO $ return)
    agentName (AgentSimple tn _) = printf "AgentSimple(|tn|=%s::%s)" (showNetDims tn) (showNetName tn)
    applyAgent agent g p = do
      let mv = moves g p
          mv'evaled = map ((evaluateGame agent p) &&& id) mv
          best'moves = map snd $ head $ reverse $ groupBy (\a b -> fst a == fst b) $ sortBy (comparing fst) $ mv'evaled

      when (null best'moves) (fail "AgentSimple: Stuck, no moves left.")
      pickList (gen agent) best'moves

instance (NeuralNetwork nn) => Agent2Eval (AgentSimple nn) where
    evaluateGame (AgentSimple nn _) P1 game = evalGameNetwork nn game
    evaluateGame agent P2 game = evaluateGame agent P1 (invertGame game)

instance (NeuralNetwork nn) => Agent2 (AgentGameTree nn) where
    type AgentParams (AgentGameTree nn) = (nn, Int)
    mkAgent (tn,depth) = return $ AgentGameTree tn depth
    agentName (AgentGameTree tn depth) = printf "AgentGameTree(d=%d, tn=%s::%s)" depth (showNetDims tn) (showNetName tn)
    applyAgent (AgentGameTree nn depth) g p = do
      let gs = GameStateGen { gsgGame = g
                            , gsgEvalSelf = eval
                            , gsgPlayerBase = p
                            , gsgPlayerNow = p
                            }

          eval gameSt = fixSign (gsgPlayerNow gameSt) 
                        $ doubleToEvalInt
                        $ evalGameNetwork nn
                        $ fixGame (gsgPlayerBase gameSt)
                        $ gsgGame gameSt 
          (princ, _score) = GTreeAlgo.negascout gs depth
          -- fixme: make sure this is correct.
          fixSign P1 v = v
          fixSign P2 v = negate v

          fixGame P1 gm = gm
          fixGame P2 gm = invertGame gm

      return (gsgGame $ head $ tail $ princ)

mkAgentSimpleFile :: FilePath -> ThrLocIO (AgentSimple TNetwork)
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
      
      best'moves <- takeBest 1 `fmap` mapM myeval mv

      when (null best'moves) (fail "AgentMCTS: Stuck, no moves left.")
      return (head best'moves)

instance (Agent2 a, Agent2Eval a) => Agent2 (AgentParMCTS a) where
    type AgentParams (AgentParMCTS a) = (Double, Int, (AgentParams a))
    mkAgent (bias, games, sub) = AgentParMCTS bias games <$> mkAgent sub <*> (withSystemRandom $ asGenIO $ return)
    agentName (AgentParMCTS eps g sub _) = printf "AgentParMCTS(g=%d,eps=%f,sub=%s)" g eps (agentName sub)
    applyAgent (AgentParMCTS bias games agEval rgen) g p = do
      agRndSkew@(AgentRandomSkew _ _ _) <- mkAgent (bias, agEval, rgen)
      let mv = moves g p
          myeval move = do
            winners <- replicateM games (winner `fmap` randomGame move)
            let count = length $ filter (==(Just p)) winners
            return (count, move)
          randomGame game = driverG2 game agRndSkew agRndSkew (GameDriverCallback (\_ -> return ()) (\_ _ -> return True))
          takeBest n some'moves = take n $ reverse $ map snd $ sortBy (comparing fst) $ some'moves
      printTL ("AgentParMCTS::moves", length mv)
      best'moves <- takeBest 1 `fmap` mapM myeval mv

      when (null best'moves) (fail "AgentMCTS: Stuck, no moves left.")
      return (head best'moves)



pickList :: GenIO -> [a] -> IO a
pickList rgen xs = do
          pick <- uniformR (0, (length xs) - 1) rgen
          return (xs !! pick)

pickListWeighted :: GenIO -> [(Double, a)] -> IO a
pickListWeighted rgen xs = do
          let wSum = sum (map fst xs)
              xsCum = scanl1 (\ (w0,_x0) (w1,x1) -> (w0+w1, x1)) xs
          p <- uniformR (0, wSum) rgen
          return $ snd $ head $ dropWhile (\(w,_) -> w < p) xsCum

evalGameNetwork :: (NeuralNetwork nn, Game2 g, Repr (GameRepr g)) => nn -> g -> Double
evalGameNetwork tn g = V.sumElements $ computeNetworkSigmoid tn (toReprNN g)

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
driverG2 :: (Game2 g, Repr (GameRepr g), Agent2 a1, Agent2 a2) => g -> a1 -> a2 -> (GameDriverCallback g) -> ThrLocIO g
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
                  -> ThrLocIO ()
sampleRandomGames canContinue prob cbChosen = do
  mygen <- withSystemRandom $ asGenIO $ return  
  agRnd <- mkAgent () :: ThrLocIO AgentRandom

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
                       -> ThrLocIO ()
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
                         -> ThrLocIO ()
sampleRandomGameDepth canContinue cbFinished = do
  agRnd <- mkAgent () :: ThrLocIO AgentRandom
  sampleGameDepth agRnd agRnd canContinue cbFinished 

-- | generate a stream of games between given agents, extracted via callback. exits after calling callback given number of times.
sampleGameDepthCount :: (Repr (GameRepr g), Game2 g, Agent2 a1, Agent2 a2) 
                         => a1
                         -> a2
                         -> Int -- ^ callback call count
                         -> (g -> Int -> IO ()) -- ^ callback
                         -> ThrLocIO ()
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
                         -> ThrLocIO ()
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


