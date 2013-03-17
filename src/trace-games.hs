module Main where

import Board
import Tournament

import Control.Monad
import Text.Printf
import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time.Clock.POSIX
import System.Process

main :: IO ()
main = do
  let cutoff = 300
  threads <- getNumCapabilities
  starttime <- getPOSIXTime
  hostname <- fmap (head . words) (readProcess "hostname" [] "")
  dataHandle'dense <- openFile (printf "data/trace.dense.%s.%d.%d.csv" hostname (round starttime :: Integer) cutoff) WriteMode
  dataHandle'spars <- openFile (printf "data/trace.spars.%s.%d.%d.csv" hostname (round starttime :: Integer) cutoff) WriteMode
  dataHandle'winnr <- openFile (printf "data/trace.winnr.%s.%d.%d.csv" hostname (round starttime :: Integer) cutoff) WriteMode

  handles <- newMVar (dataHandle'dense, dataHandle'spars, dataHandle'winnr)

  let recordHistory color history = withMVar handles $ \ (h1,h2,h3) -> do
                                     let foo brd = do
                                           Board.appendBoardCSVFile brd h1
                                           Board.appendBoardCSVFileSparse brd h2
                                           hPutStrLn h3 (colorToInt color)
                                     mapM_ foo ({- dropHalf -} history)
                                     mapM_ hFlush [h1,h2,h3]

--      dropHalf xs = drop (length xs `div` 2) xs

                                     
      nameColor White = "white"
      nameColor Black = "black"
      
      colorToInt (Just Black) = "0"
      colorToInt (Just White) = "1"
      colorToInt Nothing = "2"

  let threadFunc threadNum = forever (do
                 (winner, history) <- Tournament.simpleGameTrace cutoff
                 recordHistory winner history
               )

  asyncs <- mapM (\ threadNum -> async (threadFunc threadNum)) [1..threads]
  waitAny asyncs

  putStrLn "Error occured and async thread has completed."

  hClose dataHandle'dense
  hClose dataHandle'spars
  hClose dataHandle'winnr
