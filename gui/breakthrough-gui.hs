{-# LANGUAGE ParallelListComp #-}

module Main where

import Graphics.Blank
import Control.Concurrent
import Control.Monad
import Data.Array

data Fill = FillEmpty | FillP1 | FillP2
data BG = BGLight | BGDark | BGSelected | BGPossible

data Field = Field { fFill :: Fill
                   , fBG :: BG
                   }

drawPointEv :: Event -> Canvas ()
drawPointEv e = do
  case e of
    Event _ (Just (x,y)) -> drawPoint x y
    _ -> return ()

drawPoint :: Int -> Int -> Canvas ()
drawPoint x y = do
  font "bold 20pt Mono"
  textBaseline "middle"
  textAlign "center"
  strokeStyle "rgb(240, 124, 50)"
  strokeText ("+",fromIntegral x, fromIntegral y)
  return ()

bgToStyle BGLight = "rgb(218, 208, 199)"
bgToStyle BGDark = "rgb(134, 113, 92)"
bgToStyle BGSelected = "rgb(102,153,0)"
bgToStyle BGPossible = "rgb(153,204,0)"

drawField :: (Float,Float) -> Bool -> Field -> Canvas ()
drawField baseXY@(x,y) highlight field = do
  let s2 = side/2
      s4 = side/4
  -- background
  fillStyle (bgToStyle (fBG field))
  fillRect (x,y,side,side)
  -- border
  strokeStyle "rgb(10,10,10)"
  strokeRect (x,y,side,side)
  -- fill
  let drawFill style = do
        save ()
        beginPath ()
        -- lineWidth 4
        arc ((x+s2), (y+s2), s4, 0, (2*pi), False)
        fillStyle style
        fill ()
        restore ()
  case (fFill field) of
    FillEmpty -> return ()
    FillP1 -> drawFill "rgb(250,250,250)"
    FillP2 -> drawFill "rgb(50,50,50)"

  -- highlight
  when highlight $ do
     strokeStyle "rgb(120, 210, 30)"
     strokeRect (x+side*0.1,y+side*0.1,side*0.8,side*0.8)
    
  return ()

positionToIndex :: (Int,Int) -> Maybe (Int,Int)
positionToIndex (px,py) = do
  cx <- toCoord px
  cy <- toCoord py
  return (cx, cy)
      where
        toCoord val = case (val-offset) `div` side of
                        x | x < 0 -> Nothing
                          | x >= maxTiles -> Nothing
                          | otherwise -> Just x

maxTiles, offset, side :: (Num a) => a
side = 50
maxTiles = 8
offset = 20
      

drawBoard :: [Fill] -> Canvas ()
drawBoard boardFills = do
  let pos = zip (zip boardFills (cycle [True,False,False]))
            [ (((offset + x*side),(offset + y*side)),bg) 
                  | y <- [0..maxTiles-1], x <- [0..maxTiles-1] | bg <- boardBackgrounds ]

  mapM_ (\ ((f,hl),((x,y),bg)) -> drawField (fromIntegral x, fromIntegral y) hl (Field f bg)) pos
         

data DrawingBoard = DrawingBoard (Array (Int,Int) Field)
boardBackgrounds = let xs = (take maxTiles $ cycle [BGLight, BGDark]) in cycle (xs ++ reverse xs)
newBoard = DrawingBoard $ array ((0,0), ((maxTiles-1),(maxTiles-1))) 
                          [ ((x,y),(Field f bg)) | y <- [0..maxTiles-1], x <- [0..maxTiles-1] 
                                             | bg <- boardBackgrounds
                                             | f <- cycle [FillEmpty, FillP1, FillP2, FillP2, FillP1]
                          ]

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
         -- ref <- newIORef newBoard

         send context (drawBoard (cycle [FillEmpty, FillP1, FillP2, FillP2, FillP1]))
         moveQ <- events context MouseMove
         downQ <- events context MouseDown
         
         forkIO $ forever $ do
           evnt <- readEventQueue moveQ
           case jsMouse evnt of
             Nothing -> return ()
             Just xy -> print (evnt, positionToIndex xy)
           send context (drawPointEv evnt)

         forkIO $ forever $ do
           evnt <- readEventQueue downQ
           case jsMouse evnt of
             Nothing -> return ()
             Just xy -> print (evnt, positionToIndex xy)
           send context (drawPointEv evnt)

         return ()
