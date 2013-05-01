{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module HistogramGeneric where

import GenericGame
import AgentGeneric

import Graphics.Histogram
import Data.IORef

-- | produce histogram with game depths from random playouts
histogramRandomGameDepth :: (Repr (GameRepr g), Game2 g) 
                            => g -- ^ unused type argument for fixing game type
                            -> FilePath -- ^ where to store the plot or empty for in-memory display
                            -> Int -- ^ how many games to play
                            -> IO ()
histogramRandomGameDepth g'unused plotPath countGames = do
  let ofType :: a -> a -> b -> b
      ofType _ _ b = b

  countR <- newIORef 0
  gamesR <- newIORef []
  let appendR c = modifyIORef gamesR ((fromIntegral c):)
      incR = modifyIORef countR (1+)

  sampleRandomGameDepth ((<countGames) `fmap` readIORef countR)
                        (\ g depth -> appendR (ofType g g'unused depth) >> incR)

  depths <- readIORef gamesR
  let hist = histogram binSturges depths
  plot plotPath hist

  return ()