module Main where

import GenericGame
import AgentGeneric
import BreakthroughGame

import Data.Default
import Data.IORef
import Data.List (sort, group, groupBy)

import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async

params :: GameParams Breakthrough
params = (6,6) -- board size

main = do
  tCount <- getNumCapabilities
  let gCount = 100000
      gtCount = gCount `div` tCount
      gtCountExtra = gCount - (gtCount * tCount)

      -- first thread do extra jobs
      ttGCount 1 = gtCountExtra + gtCount
      ttGCount _ = gtCount

      threadJob tNum = do
         a1 <- mkAgent () :: IO AgentRandom
         a2 <- mkAgent () :: IO AgentRandom
         winners <- sequence $ replicate (ttGCount tNum) (oneGame a1 a2)
         return winners

  asyncThreads <- mapM (async . threadJob) [1..tCount]
  all'winners <- mapM wait asyncThreads
  let counts = map (head &&& length) $ group $ sort $ concat all'winners
  
  print counts

  return ()

oneGame :: (Agent2 a1, Agent2 a2) => a1 -> a2 -> IO (Maybe Player2)
oneGame a1 a2 = do
  ref <- newIORef Nothing

  let g0 = freshGame params :: Breakthrough
      cb :: GameDriverCallback Breakthrough
      cb = GameDriverCallback { gameTurn = (\ g p -> do
                               -- putStrLn (prettyPrintGame (g :: Breakthrough))
                               return True)
               , gameFinished = (\ g -> do
                               let w = winner g
                               putStrLn $ "Game finished! Winner: " ++ show w
                               -- putStrLn (prettyPrintGame (g :: Breakthrough))
                               writeIORef ref w

                                )
               }

  


  driverG2 g0 a1 a2 cb

  readIORef ref
