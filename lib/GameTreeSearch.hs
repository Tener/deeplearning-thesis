module GameTreeSearch where

import Data.Tree

class Game state where
    terminal :: state -> Bool
    score :: state -> Double
    children :: state -> [state]

-- unfoldGameTree :: (Game state) => 
unfoldGameTree startingState = unfoldTree (\ state -> (node state, children state)) startingState
    where
      node state = (state, score st)

minimax depth (Node (state, score) sub) | depth <= 0 = score         
                                        | isTerminal state = score    
                                        | otherwise = maximumBy (negate . minimax (depth-1)) sub


--                                                      
-- node = (board, score)                                
-- moves :: node -> [node]                              
--                                                      
--                                                      
-- mktree :: (GameTree alg) => alg -> Tree (GT alg)     
--                                                      
-- search 0 node = node                                 
-- search n node = search (n-1) (minimumBy score moves) 
--                                                      
-- score :: node -> double                              
-- scoureRec :: depth -> node -> double                 
--                                                      
-- function integer minimax(node, depth)                
--     if node is a terminal node or depth <= 0:        
--         return the heuristic value of node           
--     α = -∞                                           
--     for child in node:                       # evaluation is identical for both players 
--         α = max(α, -minimax(child, depth-1))         
--     return α                                         
--                                                      
-- ---                                                  
--                                                      
-- -- queue, history, stack                             
--                                                      
-- minimax node depth | depth <= 0 = score node         
--                    | isTerminal node = score node    
--                    | otherwise = maximumBy (minimax  
