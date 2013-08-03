{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FlexibleInstances, FlexibleContexts #-}

module AgentGenericHeuristic where

data AgentHeuristic heurFilter = AgentHeuristic heurFilter

instance (HeuristicFilter hf) => Agent2 (AgentHeuristic hf) where
    type AgentParams (AgentHeuristic hf) = hf
    mkAgent hf = AgentHeuristic <$> pure hf
    agentName (AgentHeuristic hf) = "AgentHeuristic (" ++ showHF hf ++ ")"
    applyAgent agent g p = do
      let mv = moves g p
      when (null mv) (fail "AgentHeuristic: Stuck, cant do anything.")
      let filtered = filterGood hf p g mv
      when (null filtered) (fail "AgentHeuristic: Stuck (filtered), cant do anything.")
      pickList (gen agent) filtered

class HeuristicFilter h where
  -- | filter states, leaving only good ones
  filterGood :: (Game2 g) => h -> Player2 -> g -> [g] -> [g]
  showHF :: h -> String

data HAlwaysKill = HAlwaysKill
data HAlwaysWin = HAlwaysWin

instance (HeuristicFilter a, HeuristicFilter b) => HeuristicFilter (a,b) where
  filterGood (a,b) pl gb gr = let g1 = filterGood a pl gb gr
                                  g2 = filterGood b pl gb g1
                              in g2
---
  
instance (AlwaysKill g) => HeuristicFilter HAlwaysKill g where
  filterGood :: h -> Player2 -> g -> [g] -> [g]
  filterGood _ pl gb gr = filter (isKillMove pl gb) gr  

class AlwaysKill a where
  isKillMove :: Player2 -> a -> a -> Bool

instance AlwaysKill Breakthrough where
  isKillMove P1 g1 g2 = (countP2 g1 - countP2 g2) > 0 
  isKillMove P2 g1 g2 = (countP1 g1 - countP1 g2) > 0

instance AlwaysKill g where
  isKillMove _ _ _ = False

---
  
instance (AlwaysWin g) => HeuristicFilter HAlwaysWin g where
  filterGood :: h -> Player2 -> g -> [g] -> [g]
  filterGood _ pl gb gr = filter (isWinMove pl gb) gr  

instance (Game2 g) => AlwaysWin g where
  isWinMove pl _ g = winner g == Just pl

  
