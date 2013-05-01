{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module HistogramGeneric where

import GenericGame
import AgentGeneric

import Graphics.Histogram
import Data.IORef
import System.Process

import qualified Graphics.Gnuplot.Frame.OptionSet as Opts


-- | produce histogram with game depths from random playouts
histogramRandomGameDepth :: (Repr (GameRepr g), Game2 g) 
                            => g -- ^ type argument for fixing game type. might be used (i.e. dont pass undefined)
                            -> FilePath -- ^ where to store the plot or empty for in-memory display
                            -> Int -- ^ how many games to play
                            -> IO ()
histogramRandomGameDepth g plotPath countGames = do
  let ofType :: a -> a -> b -> b
      ofType _ _ b = b

  countR <- newIORef 0
  gamesR <- newIORef []
  let appendR c = modifyIORef gamesR ((fromIntegral c):)
      incR = do
        modifyIORef countR (1+)
        print =<< readIORef countR


  sampleRandomGameDepth ((<countGames) `fmap` readIORef countR)
                        (\ gr depth -> appendR (ofType gr g depth) >> incR)

  depths <- readIORef gamesR
  let hist = histogram binSturges depths
      opts = Opts.title ("Random game depth: " ++ (gameName g)) $ 
             defOpts hist
  plotAdv plotPath opts hist
  system "sed 's/png;/png size 1000,1000;/' curve.gp > curve-fix.gp"
  system "gnuplot curve-fix.gp"
  system "rm -f curve0.csv curve.gp curve-fix.gp"

  return ()