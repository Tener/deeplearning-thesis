module Main where

import Agent
import Control.Monad

main = forever (Agent.game >> putStrLn "-----------------------------------")