{-# LANGUAGE TypeFamilies #-}

-- proper Monte Carlo Tree Search (MCTS) implementation

module MCTS where

import Control.Applicative
import Control.Arrow hiding (loop)
import Data.Default
import Data.List (sortBy)
import Data.Ord
import Data.Maybe (fromMaybe)
import System.Random.MWC
import Text.Printf

import GenericGame
import AgentGeneric

-- | game tree, nodes annotated with game evaluation statistics and game state, leafs expanded upon selection.
data Stats = Stats { visitCount :: Int
                   , totalScore :: Double
                   } deriving (Show)

data GameTree game = TNode Stats Player2 game [GameTree game] 
                   | TLeaf Player2 game
                     deriving (Show)

instance Functor GameTree where
    fmap f (TNode s p g gs) = TNode s p (f g) (fmap (fmap f) gs)
    fmap f (TLeaf p g) = TLeaf p (f g)

-- data GameGraphEl game = GNode Stats | GLeaf
-- data GameGraph game = GameGraph (Map game (GameGraphEl game)) 

data AgentProperMCTS'Graph = AgentProperMCTS'Graph
data AgentProperMCTS'Tree subAgent = AgentProperMCTS'Tree Int Double GenIO subAgent (AgentRandomSkew subAgent)

mkGameTreeStats :: Game2 game => Stats -> game -> Player2 -> GameTree game
mkGameTreeStats stat g p = TNode stat p g (map (TLeaf (nextPlayer p)) (moves g p))
mkGameTree :: Game2 game => game -> Player2 -> GameTree game
mkGameTree g p = mkGameTreeStats def g p

instance Default Stats where
    def = Stats 0 0

instance (Agent2 a, Agent2Eval a) => Agent2 (AgentProperMCTS'Tree a) where
    type AgentParams (AgentProperMCTS'Tree a) = (Int, Double, (AgentParams a))
    mkAgent (gamesCount, epsilon, paramsSub) = do
      rgen <- (withSystemRandom $ asGenIO $ return)
      sub <- mkAgent paramsSub
      skew <- mkAgent (epsilon,sub,rgen)
      return (AgentProperMCTS'Tree gamesCount epsilon rgen sub skew) 
    agentName (AgentProperMCTS'Tree g e _ sub _) = printf "AgentProperMCTS'Tree(g=%d,eps=%f,sub=%s)" g e (agentName sub)
    
    applyAgent (AgentProperMCTS'Tree gamesCount _ rgen _ agRndSkew) gameBase playerBase = 
     selectFromTree <$> iterateMaybeAct gamesCount mcts initialTree
      where
        selectFromTree (TLeaf _ _) = error "selectFromTree: TLeaf"
        selectFromTree (TNode _ _ _ sub) | null sub = error "selectFromTree: stuck (no children)"
                                         | otherwise = snd $ head $ sortBy (comparing fst) $ map (scoreStat &&& getGame) sub
        scoreStat (TLeaf _ _) = 1/0
        scoreStat (TNode (Stats vc ts) _ _ _) = negate (ts / (fromIntegral vc))
        getGame (TLeaf _ g) = g
        getGame (TNode _ _ g _) = g

        iterateMaybeAct :: Int -> (a -> IO (Maybe a)) -> a -> IO a
        iterateMaybeAct n action acc | n <= 0 = return acc
                                     | otherwise = (fromMaybe acc <$> (action acc)) >>= iterateMaybeAct (n-1) action

        -- initialTree :: (GameTree myGameType)
        initialTree = mkGameTree gameBase playerBase
        
        -- mcts :: (GameTree myGameType) -> IO (GameTree myGameType)
        mcts tree = (snd <$>) <$> selectTree tree
          where
            -- helper functions
            modifyRandomElement :: (el -> IO (Maybe (extra, el))) -> [el] -> IO (Maybe (extra, [el]))
            modifyRandomElement action lst = do
              ix <- uniformR (0, (length lst) - 1) rgen
              let (prev, (el:rest)) = splitAt ix lst
              res <- action el
              case res of
                Nothing -> return Nothing
                Just (extra, elNew) -> return (Just (extra, (prev ++ (elNew:rest))))
            
            -- selection: find leaf node in tree.
            -- selectTree :: GameTree myGameType -> IO (GameTree myGameType) 
            selectTree gtOrig = selectTreeAux gtOrig
              where
                selectTreeAux (TLeaf player game) = Just `fmap` expandTree player game
                selectTreeAux (TNode stats player game children) | null children = return Nothing
                                                                 | otherwise = do
                  res <- modifyRandomElement selectTreeAux children
                  case res of
                    Nothing -> return Nothing
                    Just (upd, newchildren) -> return (Just (upd, (TNode (upd stats) player game newchildren)))
            -- expand: replace current leaf by regular node, add fresh leaf nodes.
            -- expandTree :: Player2 -> myGameType -> ((Stats -> Stats), GameTree myGameType)
            expandTree player game = do
              score <- randomPlayout game
              -- backpropagate: update tree with new simulation data. for GameTree we do it while exiting from recursion, in GameGraph we search for preceding items. 
              -- 'update' function is applied to whole path from root to leaf. 
              let update (Stats cnt val) = Stats (cnt+1) (val+score)
                  newNode = mkGameTreeStats (update def) game player
              return (update, newNode)
            
            -- play-out: play mostly-random playouts, but with bias towards good moves (for either player)
            -- randomPlayout :: myGameType -> IO Double -- (score)
            randomPlayout game = (scoreFinishedGame . winner) <$> 
                                 driverG2 game agRndSkew agRndSkew (GameDriverCallback (\_ -> return ()) (\_ _ -> return True))
            scoreFinishedGame Nothing = 0
            scoreFinishedGame (Just p) | p == playerBase = 1
                                       | otherwise = -1


