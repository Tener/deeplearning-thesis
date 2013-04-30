module Main where

import GenericGame
import AgentGeneric
import BreakthroughGame

import Data.Default

params :: GameParams Breakthrough
params = (6,6) -- board size

main :: IO ()
main = do
  let g0 = freshGame params :: Breakthrough
      cb = def

  a1 <- mkAgent () :: IO AgentRandom
  a2 <- mkAgent () :: IO AgentRandom

  driverG2 g0 a1 a2 cb
