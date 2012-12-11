module Main where

import Agent
import Board
import qualified CairoRender

import Clocked
import Control.Monad
import Text.Printf
import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time.Clock.POSIX
import System.Process

main = do
  let cutoff = 300
  threads <- getNumCapabilities
  starttime <- getPOSIXTime
  hostname <- fmap (head . words) (readProcess "hostname" [] "")
  dataHandle <- newMVar =<< openFile (printf "data/games.%s.%d.%d-inc.csv" hostname (round starttime :: Integer) cutoff) WriteMode
  let threadFunc threadNum = forever (do
                 d0 <- getTimeDouble 
                 brd <- Agent.game (cutoff*threadNum)
                 CairoRender.saveBoard brd "last-game.svg"
                 withMVar dataHandle (Board.appendBoardCSVFile brd)
                 d1 <- getTimeDouble
                 putStrLn (printf "[%d] Time elapsed: %f seconds" (threadNum :: Int) (d1-d0))
                 >> putStrLn "-----------------------------------"
               )

  asyncs <- mapM (\ threadNum -> async (threadFunc threadNum)) [1..threads]
  waitAny asyncs

  putStrLn "Error occured and async thread has completed."
  
