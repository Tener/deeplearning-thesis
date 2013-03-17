{-# LANGUAGE CPP #-}

module CairoRender where

import Board
import Text.Printf
import System.Directory
import System.FilePath

#ifdef CAIRO

import Graphics.Rendering.Cairo
import Math.Geometry.GridMap as GridMap
import System.Process (rawSystem)

drawBoard :: Board -> Render ()
drawBoard brdNew = do

  let brd = boardToBoardOld brdNew
      side = 25
      x0 = 300
      y0 = 300
      board'indices = indices brd

      fillWhite = setSourceRGB 0.98 0.98 0.98
      fillBlack = setSourceRGB 0 0 0
      fillEmpty = setSourceRGB 0.8 0.78 0.78

      getColor xy = case GridMap.lookup xy brd of
                      Nothing -> fillEmpty -- error?
                      Just Nothing -> fillEmpty
                      Just (Just Black) -> fillBlack
                      Just (Just White) -> fillWhite

  let positions = zip board'indices
                      [ let x = fromIntegral x'
                            y = fromIntegral y'
                        in ((x0+side*((sqrt 3)*x + (sqrt 3 / 2) * y)) 
                           ,(y0+side*((3/2)*y))) | (x',y') <- board'indices ]
  -- hexes
  mapM_ (\ (xy,(x,y)) -> drawHex x y side (getColor xy)) positions

  -- text
  mapM_ (\ (xy,(x,y)) -> do
                            moveTo (x-((3/2)*side)) (y+(sqrt 3 / 2)*side)
                            setSourceRGB 0 0 1
                            showText (show xy)) positions


--  sequence_ [ let x = fromIntegral x'
--                  y = fromIntegral y'
--                  title = show (x',y')
--              in drawHex title 
--                         (x0+side*((sqrt 3)*x + (sqrt 3 / 2) * y)) 
--                         (y0+side*((3/2)*y)) 
--                         side | (x',y') <- board'indices
--            --          board'indices
--            ]

drawHex :: Double -> Double -> Double -> Render a -> Render ()
drawHex x y s filler = do
  save

  moveTo x y

  setSourceRGB 0 1 1
  relMoveTo 0 s
  rotate 1.04719755
 
  relLineTo 0 s
  rotate 1.04719755
 
  relLineTo 0 s
  rotate 1.04719755
 
  relLineTo 0 s
  rotate 1.04719755
 
  relLineTo 0 s
  rotate 1.04719755
 
  relLineTo 0 s
  rotate 1.04719755
 
  relLineTo 0 s
  rotate 1.04719755

  setSourceRGB 0.9 0.2 0.3

  -- wypełnienie hexów
  _ <- filler

  fillPreserve

  -- hexy
  restore
  stroke
  
drawTest :: IO ()
drawTest = draw starting'board'default 0

saveBoard :: Board -> FilePath -> IO ()
saveBoard brd name = do
  withSVGSurface name 800 600 (\ surf -> renderWith surf (drawBoard brd))

draw :: Board -> Int -> IO ()
draw brd num = do
  tmp <- getTemporaryDirectory
  let fname = tmp </> printf "abalone-draw.tmp.%04d.svg" (num :: Int)
  saveBoard brd fname

view :: Board -> IO ()
view brd = do
  tmp <- getTemporaryDirectory
  let name = tmp </> "board-abalone-view.svg"
  saveBoard brd name
  rawSystem "eog" [name]
  return ()

#else

saveBoard brd name = do
  writeFile name (show brd)

draw brd num = do
  tmp <- getTemporaryDirectory
  let fname = tmp </> printf "abalone-draw.tmp.%04d.txt" (num :: Int)
  saveBoard brd fname


#endif
