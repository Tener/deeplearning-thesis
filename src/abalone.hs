{-# LANGUAGE BangPatterns #-}

module Main where

import Board
import Tournament
import qualified CairoRender

-- import Clocked
import Control.Monad
import Text.Printf
import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time.Clock.POSIX
import System.Process

main = do
  let cutoff = 700
  threads <- getNumCapabilities
  starttime <- getPOSIXTime
  hostname <- fmap (head . words) (readProcess "hostname" [] "")
  dataHandle <- openFile (printf "data/games.%s.%d.%d-inc.csv" hostname (round starttime :: Integer) cutoff) WriteMode
  dataHandle'sparse <- openFile (printf "data/games.%s.%d.%d-inc-sparse.csv" hostname (round starttime :: Integer) cutoff) WriteMode

  handles <- newMVar (dataHandle, dataHandle'sparse)

  let threadFunc threadNum = forever (do
--                 d0 <- getTimeDouble 
                 !brd <- Tournament.simpleGame (cutoff*threadNum)
                 withMVar handles (\ (h1,h2) -> do
                                     -- CairoRender.saveBoard brd "last-game.svg"
                                     Board.appendBoardCSVFile brd h1
                                     Board.appendBoardCSVFileSparse brd h2
                                  )
--                 d1 <- getTimeDouble
--                 putStrLn (printf "[%d] Time elapsed: %f seconds" (threadNum :: Int) (d1-d0))
                 >> putStrLn "-----------------------------------"
               )

  asyncs <- mapM (\ threadNum -> async (threadFunc threadNum)) [1..threads]
  waitAny asyncs

  putStrLn "Error occured and async thread has completed."

  hClose dataHandle
  hClose dataHandle'sparse
