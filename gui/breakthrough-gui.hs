{-# LANGUAGE ParallelListComp, OverloadedStrings #-}

module Main where

import Web.Scotty

import Graphics.Blank
import Control.Concurrent
import Control.Monad
import Data.Array
import Data.Maybe
-- import Data.Binary (decodeFile)

import System.Environment
import System.FilePath

import BreakthroughGame
import GenericGame
import ThreadLocal
import AgentGeneric
import MinimalNN

import qualified Data.HashMap as HashMap

import Text.Printf

data Fill = FillEmpty | FillP1 | FillP2
data BG = BGLight | BGDark | BGSelected | BGPossible deriving (Eq,Read,Show,Ord)
data DrawingBoard = DrawingBoard { getArrDB :: (Array (Int,Int) Field) }

data Field = Field { fFill :: Fill
                   , fBG :: BG
                   , fSuperBG :: Maybe BG
                   }

-- | enable feedback on moving mouse. works poorly with big latency links.
enableMouseMoveFeedback :: Bool
enableMouseMoveFeedback = False

-- | path where static directory resides
staticDataPath :: FilePath
staticDataPath = "."

-- game params
maxTiles, offset, side :: (Num a) => a
side = 50
maxTiles = 8
offset = 50
      


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
  let actualBG = fromMaybe (fBG field) (fSuperBG field)
  fillStyle (bgToStyle actualBG)
  fillRect (x,y,side,side)
  -- border
  strokeStyle "rgb(10,10,10)"
  strokeRect (x,y,side,side)
  -- fill
  let drawFill style1 style2 = do
        save
        beginPath
        -- lineWidth 4
        let px = x+s2
            py = y+s2
        arc (px,py, s4, 0, (2*pi), False)
        custom $ unlines $ [
              printf "var grd=c.createRadialGradient(%f,%f,3,%f,%f,10); " px py px py
             ,printf "grd.addColorStop(0,%s);                     " (show style1)
             ,printf "grd.addColorStop(1,%s);                     " (show style2)
             ,"c.fillStyle=grd;                                   "
             ]

        fill
        restore
  case (fFill field) of
    FillEmpty -> return ()
    FillP1 -> drawFill "rgb(250,250,250)" "rgb(240,240,240)"
    FillP2 -> drawFill "rgb(50,50,50)" "rgb(40,40,40)"

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

drawFills :: [Fill] -> Canvas ()
drawFills boardFills = do
  let pos = zip (zip boardFills (cycle [True,False,False]))
            [ (((offset + x*side),(offset + y*side)),bg) 
                  | y <- [0..maxTiles-1], x <- [0..maxTiles-1] | bg <- boardBackgrounds ]

  mapM_ (\ ((f,hl),((x,y),bg)) -> drawField (fromIntegral x, fromIntegral y) hl (Field f bg Nothing)) pos
         

boardBackgrounds = let xs = (take maxTiles $ cycle [BGLight, BGDark]) in cycle (xs ++ reverse xs)
ixToBackground (x,y) = if ((x-y) `mod` 2) == 0 then BGLight else BGDark

newDemoBoard = DrawingBoard $ array ((0,0), ((maxTiles-1),(maxTiles-1))) 
                              [ ((x,y),(Field f bg Nothing)) | y <- [0..maxTiles-1], x <- [0..maxTiles-1] 
                                             | bg <- boardBackgrounds
                                             | f <- cycle [FillEmpty, FillP1, FillP2, FillP2, FillP1]
                              ]


drawBoard maybeHighlightPos (DrawingBoard arr) = mapM_ (\ (pos,field) -> drawField (fi pos) (hl pos) field) (assocs arr) 
    where
      hl p = Just p == maybeHighlightPos 
      fi (x,y) = (offset+side*(fromIntegral x), offset+side*(fromIntegral y))

drawBreakthroughGame :: Breakthrough -> DrawingBoard
drawBreakthroughGame br = let (w,h) = boardSize br
                              toFill Nothing = FillEmpty
                              toFill (Just P1) = FillP1
                              toFill (Just P2) = FillP2
                              getFill pos = toFill $ HashMap.lookup pos (board br)
                              arr = array ((0,0), (w-1,h-1))
                                    [ ((x,y),(Field (getFill (x,y)) (ixToBackground (x,y)) Nothing)) | y <- [0..w-1], x <- [0..h-1]]
                              result = DrawingBoard arr
                          in result
                              

data CanvasGameState = CanvasGameState { boardDrawn :: DrawingBoard
                                       , lastHighlight :: (Maybe Position)
                                       , boardState :: Breakthrough
                                       , playerNow :: Player2
                                       , allFinished :: Bool
                                       }

makeCGS b p = CanvasGameState (drawBreakthroughGame b) Nothing b p False
drawUpdateCGS ctx cgs getPName = send ctx $ do
   drawBoard (lastHighlight cgs) (boardDrawn cgs)
   let p = playerNow cgs
   drawCurrentPlayer p (getPName p)
   let win = winner (boardState cgs)
   case win of
     Nothing -> return ()
     Just w -> drawWinner w
   return (cgs { allFinished = (win /= Nothing) })

p2Txt :: Player2 -> String
p2Txt P1 = "Player 1"
p2Txt P2 = "Player 2"

drawWinner w = do
  let txt = p2Txt w ++ " wins the game!"
      tpx = offset + (maxTiles * side / 2)
      tpy = offset + (maxTiles * side / 2)
      
      rpx = offset
      rpy = offset
      rdimx = maxTiles * side
      rdimy = maxTiles * side

  globalAlpha 0.75
  fillStyle "gray"
  fillRect (rpx,rpy,rdimx,rdimy)
  globalAlpha 1
  textBaseline "middle"
  textAlign "center"
  font "bold 20pt Sans"
  strokeStyle (if w == P1 then "darkred" else "darkgreen")
  strokeText (txt, tpx, tpy)
  
drawCurrentPlayer pl plName = do
  -- put text
  let txt = printf ("Current move: %s (%s)"::String) (p2Txt pl) plName
  font "15pt Sans"
  clearRect (0,0,1500,offset*0.9) -- fix for damaging board border
  fillStyle "black"
  fillText (txt, offset, offset/2)
  -- put symbol on the left side
  clearRect (0,0,offset*0.9,1500)
  let px = offset/2
      py = offset + (side * pside)
      pside = 0.5 + if pl == P1 then 0 else (maxTiles-1)
      pcol = if pl == P1 then "red" else "blue"
  save
  font "bold 20pt Mono"
  textBaseline "middle"
  textAlign "center"
  strokeStyle "rgb(240, 124, 50)"
  custom $ unlines $ [
              printf "var grd=c.createRadialGradient(%f,%f,3,%f,%f,10); " px py px py
             ,printf "grd.addColorStop(0,%s);                           " (show pcol)
             ,"grd.addColorStop(1,\"white\");                           "
             ,"c.strokeStyle=grd;                                       "
             ]
  strokeText ("+",px,py)
  restore

main :: IO ()
main = do
  -- crude command line parsing
  args <- getArgs
  let port = case args of
               (x:_) -> read x
               _ -> 3000
  print ("port",port)

  let dbn = case args of
              (_:fn:_) -> fn
              _ -> "assets/dbn.bin"
  print ("dbn",dbn)
  network <- MinimalNN.decodeFile dbn
  
  -- apps
  let app1 = ("/pvc", (pvc network))
      app2 = ("/pvp", pvp)
  apps <- mapM (\ (path, app) -> blankCanvasParamsScotty app staticDataPath False path) [app1, app2]
  
  -- main page
  let index = get "/" (file (staticDataPath </> "static" </> "global-index.html"))
                
  -- launch server
  scotty port (sequence_ (apps ++ [index]))

pvc :: TNetwork -> Context -> IO ()
pvc network context = do
  let agent'0 = runThrLocMainIO (mkTimed "wtf3" network) :: IO (AgentTrace (AgentSimple TNetwork))
      agent'1 = runThrLocMainIO (mkTimed "rnd" ()) :: IO (AgentTrace AgentRandom)
      agent'2 = runThrLocMainIO (mkTimed "wtf2" 1000) :: IO (AgentTrace AgentMCTS)
--      agent'3 = runThrLocMainIO (mkTimed "tree" (network, 4)) :: IO (AgentTrace AgentGameTree)
      agent'4 = runThrLocMainIO (mkTimed "wtf" (2, 50, network)) :: IO (AgentTrace (AgentParMCTS (AgentSimple TNetwork)))

--  let agent'0 = runThrLocMainIO (mkAgent network) :: IO ((AgentSimple TNetwork))
--      agent'1 = runThrLocMainIO (mkAgent ()) :: IO (AgentRandom)
--      agent'2 = runThrLocMainIO (mkAgent 1000) :: IO (AgentMCTS)
--      agent'3 = runThrLocMainIO (mkAgent (network, 4)) :: IO (AgentGameTree)
--      agent'4 = runThrLocMainIO (mkAgent (2, 50, network)) :: IO ((AgentParMCTS (AgentSimple TNetwork)))

  agent <- agent'0
   
  let initial = makeCGS br P1
      br = freshGame (maxTiles,maxTiles) :: Breakthrough
      getPlayerName P1 = "human"
      getPlayerName P2 = agentName agent
      drawCGS' cgs = drawUpdateCGS context cgs getPlayerName
  var <- newMVar =<< drawCGS' initial

  let drawMove mPos = modifyMVar_ var $ \ cgs -> if allFinished cgs then return cgs else do
        let prevPos = lastHighlight cgs
        when (mPos /= prevPos) (send context (drawBoard mPos (boardDrawn cgs)))
        return (cgs { lastHighlight = mPos })

      autoPlay cgs | allFinished cgs = return cgs
                   | otherwise = do
        let board = boardState cgs
            player = playerNow cgs
        newBoard <- runThrLocMainIO (applyAgent agent board player)
        drawCGS' (makeCGS newBoard (nextPlayer player))

      clearSuperBG (Field f bg _) = (Field f bg Nothing)
      lastSelect cgs = case filter (\ (pos,(Field _ _ sup)) -> sup == Just BGSelected) (assocs (getArrDB $ boardDrawn cgs)) of
                         [(pos,_)] -> Just pos
                         _ -> Nothing -- no matches or more than one match

      clickSelect ix cgs = do
        let DrawingBoard brd = boardDrawn cgs
            brdClean = fmap clearSuperBG brd
            brd' = accum (\ (Field f bg _) sup -> (Field f bg sup)) brdClean [(ix,(Just BGSelected))] 
        send context (drawBoard (Just ix) (DrawingBoard brd'))
        return (cgs { boardDrawn = DrawingBoard brd' })

      clickClear cgs = do
        let DrawingBoard brd = boardDrawn cgs
            brd' = fmap clearSuperBG brd
        send context (drawBoard (lastHighlight cgs) (DrawingBoard brd'))
        return (cgs { boardDrawn = DrawingBoard brd' })

      drawClick Nothing = return ()
      drawClick mPos@(Just sndPos@(x,y)) = modifyMVar_ var $ \ cgs -> if allFinished cgs then return cgs else do
        let valid state = state `elem` moves (boardState cgs) (playerNow cgs)
        case lastSelect cgs of
          Nothing -> clickSelect sndPos cgs 
          Just fstPos | fstPos == sndPos -> clickClear cgs
                      | otherwise -> case applyMove (boardState cgs) (fstPos,sndPos) of
                                      Nothing -> clickSelect sndPos cgs
                                      Just newState | valid newState -> do
                                                                            newCGS <- drawCGS' (makeCGS newState (nextPlayer (playerNow cgs)))
                                                                            autoPlay newCGS
                                                    | otherwise -> clickSelect sndPos cgs


  when enableMouseMoveFeedback $ do
    moveQ <- events context MouseMove
    void $ forkIO $ forever $ do
      evnt <- readEventQueue moveQ
      case jsMouse evnt of
        Nothing -> return ()
        Just xy -> do
                drawMove (positionToIndex xy)

  downQ <- events context MouseDown
  forkIO $ forever $ do
    evnt <- readEventQueue downQ
    case jsMouse evnt of
      Nothing -> return ()
      Just xy -> do
                drawClick (positionToIndex xy)
                
 

  return ()

pvp :: Context -> IO ()
pvp context = do
  let initial = makeCGS br P1
      br = freshGame (maxTiles,maxTiles) :: Breakthrough
      drawCGS' cgs = drawUpdateCGS context cgs (const ("human" :: String))
  var <- newMVar =<< drawCGS' initial

  let drawMove mPos = modifyMVar_ var $ \ cgs -> if allFinished cgs then return cgs else do
        let prevPos = lastHighlight cgs
        when (mPos /= prevPos) (send context (drawBoard mPos (boardDrawn cgs)))
        return (cgs { lastHighlight = mPos })

      clearSuperBG (Field f bg _) = (Field f bg Nothing)
      lastSelect cgs = case filter (\ (pos,(Field _ _ sup)) -> sup == Just BGSelected) (assocs (getArrDB $ boardDrawn cgs)) of
                         [(pos,_)] -> Just pos
                         _ -> Nothing -- no matches or more than one match

      clickSelect ix cgs = do
        let DrawingBoard brd = boardDrawn cgs
            brdClean = fmap clearSuperBG brd
            brd' = accum (\ (Field f bg _) sup -> (Field f bg sup)) brdClean [(ix,(Just BGSelected))] 
        send context (drawBoard (Just ix) (DrawingBoard brd'))
        return (cgs { boardDrawn = DrawingBoard brd' })

      clickClear cgs = do
        let DrawingBoard brd = boardDrawn cgs
            brd' = fmap clearSuperBG brd
        send context (drawBoard (lastHighlight cgs) (DrawingBoard brd'))
        return (cgs { boardDrawn = DrawingBoard brd' })

      drawClick Nothing = return ()
      drawClick mPos@(Just sndPos@(x,y)) = modifyMVar_ var $ \ cgs -> if allFinished cgs then return cgs else do
        let valid state = state `elem` moves (boardState cgs) (playerNow cgs)
        case lastSelect cgs of
          Nothing -> clickSelect sndPos cgs 
          Just fstPos | fstPos == sndPos -> clickClear cgs
                      | otherwise -> case applyMove (boardState cgs) (fstPos,sndPos) of
                                      Nothing -> clickSelect sndPos cgs
                                      Just newState | valid newState -> drawCGS' (makeCGS newState (nextPlayer (playerNow cgs)))
                                                    | otherwise -> clickSelect sndPos cgs

--  -- disabled: requires unreleased null-canvas-0.2.8
--  when True $ do
--    clickQ <- events context Click
--    void $ forkIO $ forever $ do
--      evnt <- readEventQueue clickQ
--      print ("click",evnt)

  when enableMouseMoveFeedback $ do
    moveQ <- events context MouseMove
    void $ forkIO $ forever $ do
      evnt <- readEventQueue moveQ
      case jsMouse evnt of
        Nothing -> return ()
        Just xy -> drawMove (positionToIndex xy)

  downQ <- events context MouseDown
  forkIO $ forever $ do
    evnt <- readEventQueue downQ
    case jsMouse evnt of
      Nothing -> return ()
      Just xy -> drawClick (positionToIndex xy)

  return ()

