module Constraints where

import Board
-- import CommonDatatypes
import MinimalNN
import NeuralNets

import Data.Maybe
import Control.Monad
import Text.Printf
import System.Random.MWC

-- import qualified Data.Vector.Unboxed as U
import qualified Data.Packed.Vector as Vector

-- 
-- 

newtype WBoard = WBoard Board -- board evaluated from perspective of White
newtype BBoard = BBoard Board -- dual of WBoard

data Constraint = CBetter Board Board -- first board is better than the other
                | CBetterAll Board [Board] -- first board is better then ALL the others

-- | check if constraint is held under given evaluation function
checkConstraint :: (Board -> Double) -> Constraint -> Bool
checkConstraint f con = case con of 
                          CBetter b0 b1 -> f b0 > f b1
                          CBetterAll b bs -> let aux = f b in all (\b' -> aux > f b') bs

-- | score constraints, returns percentage true
scoreConstraints :: (Board -> Double) -> [Constraint] -> Double
scoreConstraints f cons = let len = length cons
                              true = length $ filter id (map (checkConstraint f) cons)
                          in (fromIntegral true) / (fromIntegral len)

-- | simple way to generate constraints, we dont do any recursion: just check immidiate moves
generateConstraintsSimple :: Board -> Board -> [Constraint]
generateConstraintsSimple brd'base brd'good = if brd'good `elem` getMoves White brd'base 
                                               then [CBetter brd'good brd | brd <- getMoves White brd'base, brd /= brd'good]
                                               else error "brd'good error"

-- | good moves

good'moves :: [(Board,Board)]
good'moves = [(b0,b1),(b2,b3)] ++ concatMap movesFromGame games'all
    where
      b0 = boardFromBalls [ b,b,b,e,e,
                            e,b,b,b,e,e,
                            e,b,b,b,e,e,e,
                            e,e,b,w,w,e,e,e,
                            e,e,e,w,w,w,e,e,e,
                            e,e,w,w,w,w,e,e,
                            e,w,e,e,w,e,e,
                            e,w,w,e,e,e,
                            e,e,e,e,e]

      b1 = boardFromBalls [ b,b,b,e,e,
                            e,b,b,b,e,e,
                            e,b,b,b,e,e,e,
                            e,e,b,w,w,e,e,e,
                            e,e,e,w,w,w,e,e,e,
                            e,e,w,w,w,w,e,e,
                            e,w,w,w,w,e,e,
                            e,e,e,e,e,e,
                            e,e,e,e,e]

--
--
-- ("SCORE",1.0)
-- ("NEURON",([[[-4.737104590217833e-2,-0.48302911622227573,0.9030714633508061,0.8418260380079878,0.8359248554011185,4.882217127284649e-3,-0.968858808797328,0.9225408744962993,-0.6799416833573841,0.4757992379547342,-0.7250531096665165,0.183630820564175,0.7445159775281176,0.21455181163382409,-0.2492868258707175,-9.99709615694131e-2,-0.802052079791906,-0.7126511362721744,-0.6479788536046971,0.35453776092936073,-0.43047591128504537,0.6849699210918445,-5.8714167310931265e-2,0.31637351374383216,-0.7234814955485807,-6.636848146748497e-2,0.22755411975261608,-0.27820396188828744,0.2910726439454232,7.993982603380334e-2,-0.571300898161544,-0.446363586184124,-0.44367658384203223,0.4873759297450364,-0.729362847195991,0.8674413052062386,-0.9912658961726348,-0.7562657344025194,-0.7343127789713411,-0.9105182765637363,0.8331842738219348,-0.15438546453173818,0.2803333555406755,-0.2502734382228995,-0.8143775866833585,0.40573356973954344,-0.8333444083329671,-2.1658936487824798e-2,-0.9086490528304558,-0.2387715879716492,-0.3187950849149772,4.973858985817814e-2,0.6648048931460957,-0.25421571690092315,-0.29448338762489423,-0.47954013473124824,0.5209566012726141,-0.36688779710059527,-0.5749490004418385,0.9508643937974017,-0.6222028149712633,0.6770034307651496,0.45734523587320886,0.9324490455794707,0.5783050509674459,1.636988441196796e-2,-0.741795899624772,-0.6519663133717117,-0.3055824911076648,-0.5617941033057832,0.164495250475998,-0.8569388681593673,-0.20025884632327418,0.4519910878256981,-0.285162555831995,-0.7987804376805763,0.6801367287136737,-2.1772311424830493e-2,-0.6859163731051454,0.5831917827268864,-0.3276567081963677,-0.3590907216291026,0.1687475760256658,-0.7803614882656282,-0.18581872761841356,-9.114892104983285e-3,0.48459241095121164,-0.91251491331153,-0.8841310831391545,-0.7122380996934312,2.448204434737966e-2,-0.40063649552663505,-0.562542297814576,-0.3491424728374366,0.4809489076722919,-0.15581283255521838,-0.27217073753148724,-7.764627081282893e-2,-0.787524765107279,-0.15686561338106597]]],[[0.0]]))


      boardFromBalls' str = boardFromBalls (catMaybes $ map charToBall str)
      charToBall 'w' = Just w
      charToBall 'b' = Just b
      charToBall 'e' = Just e
      charToBall _ = Nothing

      b2 = boardFromBalls' $ concat [ "ebeee"
                                    , "eeeeee"
                                    , "eebeeee"
                                    , "wwbbeeew",
                                      "wwbbbeeww",
                                      "weweewwe",
                                      "eweewbe",
                                      "eeewbb",
                                      "eebbb"]
                              
      b3 = boardFromBalls' $ concat [ "ebeee"
                                    , "eeeeee"
                                    , "eebeeee"
                                    , "wwbbeeew",
                                      "wwbbbeeww",
                                      "weweeewe",
                                      "eweewbe",
                                      "eeewbb",
                                      "eewbb"]
                            


      b = Just Black
      w = Just White
      e = Nothing


-- | load DBN, apply single neuron to DBN output

-- | benchmark random single-neuron networks

-- singleNeuronRandomSearch :: Double -> Int -> IO ()
singleNeuronRandomSearch target thrnum = do
  gen <- withSystemRandom $ asGenIO $ return

  let sparse = True
  (dbn,sizes) <- parseNetFromFile `fmap` readFile (if sparse then "/home/tener/abalone-nn/nn_183.txt-100" else "/home/tener/abalone-nn/nn_61.txt-100")
  print (dbn,sizes)
  let lastLayerSize :: Int
      lastLayerSize = last sizes
      
      randomNeuron :: IO ([[[Double]]], [[Double]])
      randomNeuron = do
        (weights) <- (replicateM lastLayerSize (uniformR (-1,1) gen))
        let bias = 0
        return ([[weights]],[[bias]])

  let inf = 1/0

      constraints = concat $ map (uncurry generateConstraintsSimple) good'moves

      evalNeuronBoard neuron brd = 
          let final = Vector.toList $ computeTNetworkSigmoid neuronTNet 
                                    $ computeTNetworkSigmoid dbn 
                                    $ (if sparse then boardToSparseNN else boardToDenseNN) brd 
              neuronTNet :: TNetwork
              neuronTNet = uncurry mkTNetwork neuron
             in
          case final of
            [x] -> x
            [] -> error "No values returned. What?!"
            _ -> error "Too many values returned. Last layer too big."
              
                                   
      scoreNeuron n = scoreConstraints (evalNeuronBoard n) constraints
    
      go (best'neuron,best'score) | best'score >= target = return (best'neuron,best'score)
                                  | otherwise = do
         neuron <- randomNeuron
         let score = scoreNeuron neuron
         if score > best'score 
          then do
            putStrLn (printf "[%d] NEURON %s" thrnum (show neuron))
            putStrLn (printf "[%d] SCORE %f (cnum=%d)" thrnum score (length constraints))
            go (neuron,score) 
          else go (best'neuron,best'score)
         
  go (undefined,negate inf)





----------- games played by ABA-PRO from http://www.ist.tugraz.at/aichholzer/research/rp/abalone/games.php
-- Game 	Date 	Starting 	Players 	Time 	Balls 	Winner
-- 1 	08.12.2002 	Standard 	ABA-PRO / hofnar 	3:00:00 / 1:30:00 	1 / 1 	tie
-- 2 	09.12.2002 	Belgian Daisy 	rodion / ABA-PRO 	0:10:55 / 0:14:38 	0 / 1 	unfinished
-- 3 	22.12.2002 	Belgian Daisy 	hofnar / ABA-PRO 	1:00:35 / 0:23:03 	0 / 1 	tie
-- 4 	23.12.2002 	Belgian Daisy 	ABA-PRO / winner1234 	0:09:58 / 0:17:00 	6 / 0 	ABA-PRO
-- 5 	13.01.2003 	Belgian Daisy 	badboyz / ABA-PRO 	0:13:57 / 0:06:53 	0 / 6 	ABA-PRO

-- Game 1:
--  1.a1b2 i5h5 2.a5b5 i6h6 3.b5c5 i7h7 4.b6b5 i8h8 5.b1c2 h9g8 6.a3b3 h7g7 7.b4c4 h4h5 8.a4b5a3 e6e7 9.a3a2 h6g6 10.b3c3 i9i8 11.a2b3 i8h8 12.b4b3 f8e7 13.d3d4 h7g6 14.b1b2 e4f5 15.b4c4 d7d8 16.d6c5 d8e8 17.b4b3 f8g9 18.b1b2 g9f8 19.b4b3 f5g6 20.c2d3 f8f7 21.b2c2 i8i7 22.b1a1b2 i7i8 23.a2b2a1 i8h8 24.b1b2 f5g6 25.b2c2 i8h7 26.d2c2 g5h6 27.f4e3 h6g5 28.d2c2 e6f6 29.a2b2 h6g6 30.d2c2 f8e7 31.a2b2 d6e7 32.d2c2 f5g6 33.a2b2 f8f7 34.d3d4 h7g6 35.d6c5 i8h7 36.b4c4 e6f6 37.e3e4 f4g4 38.d2e3 g4f4 39.e4e5 h6g6 40.e7d6 e9e8 41.b4c4 e6f6 42.d6c5 f4g4 43.b3c4 h6g6 44.b2b3 h7h6 45.d6c5 f7f6 46.c2d3 f4g5 47.d3e4 i7h7 48.d5e5 f7f6 49.a3b3 h7g7 50.b3c4 i5h4 51.g6g5 g3h4 52.f4g4 h6h5 53.g5f4 h4h5 54.c5d5 i8h8 55.g4f3 g5h6g6 56.b4c5 h5i5h6 57.d3e4 g8g7 58.g5f4 i6i7 ...
-- Game 2:
-- 1.a1b2 a5b5 2.i9h8 a4b4 3.i8h7 b5c5 4.h7g6 g5f4 5.c3d4d3 h4g4 6.h9g8 c5d5 7.h8g8 i6h5 8.f6f8e6 c4d59.g6f6 h6g6 10.a2b3 b6c6 11.e8f8 b4c5 12.b3b4 c6c5 13.b2c2 c4d5 14.c3c4 d5e6 15.h9h8 c5d5 16.d4d3 f7e6 17.h8g7 f4f5 18.g7f7 g4f3f4 19.f8e7 g8g7 20.b3c3 e3f4 21.b4b3 f6e6 22.d7e7 g5g6 23.f7f6 f4f5 24.f7g8f8 i5h5 25.g9f8 g6f6 26.c6b6b5 b4c5 27.f8g9e8 e6e7
-- Game 3:
-- 1.i9h8 i5h5 2.i8h7 h5g5 3.a1b2 a5b5 4.a2b3 h4g4 5.h7g6 b4c5 6.g6f5 i6h6 7.g7g8f7 b6c6 8.b3c3 a4b5 9.c4d4 d7d6 10.f4e4 b5c6 11.f7f6 c6c5 12.c1d1 g4g5 13.f8f7 c4d5 14.f4f5 g5g6 15.b1c1 g6h7 16.b2c2 h4h5 17.h9g9 h5h6 18.h9g9 h7g7 19.c1d2 d6e6 20.g6g5 h6g6 21.d1e2 g6f6 22.d4e4 g7g8 23.f9e8 c3d4 24.e4f4 h8g8 25.d8c7 h4h5 26.c2d3 g9g8 27.c7c6 h5h6 28.c6b5 h6h7 29.b5b4 d7c6 30.g4f4 h7g7 31.b4b3 g7f6 32.g5g4 g8g7 33.b3b2 c5b4 34.b2c3b1 g7f7 35.c7b6 d7c6 36.b6a5 f8e7 37.e9f9 c6c5 38.c2d2 c5d5 39.d3e4 b5c5 40.d2d3 c5d5 41.b1c2 c4c3 42.c1d2 c2c3 43.g5g4 c3c4 44.f9g9 d4e5 45.d2c2 b4c4 46.g3g4 d6d5 47.f3e3 c4c3 48.f2e2 g6f6 49.c2d3 c3b2c4 50.c1d1 b3b4 51.g9h9 e8f8 52.d1e2 d6d5 53.d2e2 b4b5 54.h9h8 e5f6 55.h5g5 d4d5 56.a5a4 h8g7 57.g4h5 g7f6 58.a4a3 f6e5 59.d1e2 d6c5 60.a3a2 e5d4 61.a1b1 f8e7 62.b1c1 e6f7e5 63.e2e3 g6f6g7 64.c1d2 b2c3 65.d2e2 g7f7 66.a2b2 e5d4 67.a1b1 b2c3 68.b1c1 c3d4 69.c1d2 b4c4b3 70.f2g3 c3b3c4 71.g7g6 d3c3 72.g6f5 d6d5 73.g3f3 c3c4 74.i9i8 b4c5
-- Game 4:
-- 1.a1b2 i5h5 2.a2b3 a5b6 3.i9h8 i6h6 4.h8g7 h6g6 5.b3c4 c5c7d6 6.b1c2 b4b6c5 7.g8g7f8 g4g5f3 8.c2d3 h4h5 9.c3d4 h6g5 10.b2c2 a4b5 11.i8h8 c5d6 12.c4c5 c7c6 13.h9h8 c5d6 14.h7h6 g7h7 15.g9g8 h4i5 16.h8g8 e7d6 17.h5h6 h8g7 18.h7h8 g7h7 19.h8g8 i5h5 20.e8e7 b5b4 21.c2c3 b4b5 22.e4d3 h5g4 23.c2c3 c7b6 24.c5d5 h7g6 25.f8e7 d8e8 26.c4d4 h4h5 27.f6e6 b6b5 28.g8f8 b4c5b3 29.f8e7 h6g5 30.f7e6 a2b2 31.c4d4 h4h5 32.c6d6 e8f8 33.h7g7 f3g4 34.g7f7 f8g8 35.f4e4 g4f4 36.e7f8 f5g6 37.f8f7 f4g4 38.d5e5 i5h4 39.d3e4 i8h8 40.e6f6 i6h5 41.c5d6d5 h4g3 42.h6g5 h5h4 43.d4e4 g3f2 44.f6f5 h8g7 45.f4f3
-- Game 5:
-- 1.i9h8 i6h62.i8h7 h5g5 3.h7g6 a5b5 4.h8g7 a4b4 5.a1b1 g5f4 6.g8g7 f4h4e3 7.b1c2 e3e4 8.a2b2 b6c6 9.g6f6 b6b5 10.b1c1 g3f3 11.f5g5f4 b3c4 12.c2d3 c6c5 13.d6e7 c5c4 14.d1d2 c4c3 15.g7g8 b4b5c4 16.d2d3 d6e6 17.e7f8 h6g5 18.f4e4 d7d6 19.g4f4 c5d6 20.d5d4 d6e6 21.h6h7 e5d5 22.e4e5 e7e6 23.h7g7 i5h4 24.b2b3 h4h5 25.h9g9 e5f6 26.g9f8 h5g5 27.h8g8 f3g4 28.f8e7 f5f6 29.g8f8 c1c2 30.d6c6 c2c3 31.d2d3 b4c5 32.d3d4 c3c4 33.c7b6 e3d3 34.f4e4 c6c5 35.b3c3 g7f6 36.c3d3 g6f6 37.c6b6b5 c5d6 38.a5b5 d6d7 39.g9g8 f8f7 40.e8f8 b4c4 41.f4e3 d8d7 42.b2c2 e6f7 43.h9h8 d6e7 44.g9h9 f7g8 

-- Game 1:

readGame :: Board -> String -> [Board]
readGame brd'start str = scanl (\ brd (col,mv)  -> runTxtMove col brd mv ) brd'start (zip (cycle [Black, White]) (words str))

a <> b = (a,b)

movesFromGame :: (Color, [Board]) -> [(Board,Board)]
movesFromGame (good, moves) = map (neg . snd) $ filter ((==good) . fst) $ zip (cycle [Black, White]) (zip moves (tail moves))
    where
      neg bs@(b1,b2) | good == Black = (negateBoard b1, negateBoard b2)
                     | otherwise = bs

-- (dobry gracz, sekwencja ruchów)
-- 1, 9, 13, 10, 12

--games'all = [game1, game2, game3, game4, game5]
--games'all = []
games'all = [game2, game3, game4, game5]

game1, game2, game3, game4, game5 :: (Color, [Board])
game1 = Black <> (take 1 $ readGame (negateBoard starting'board'default) "a1b2 i5h5 a5b5 i6h6 b5c5 i7h7 b6b5 i8h8 b1c2 h9g8 a3b3 h7g7 b4c4 h4h5 a4b5a3 e6e7 a3a2 h6g6 b3c3 i9i8 a2b3 i8h8 b4b3 f8e7 d3d4 h7g6 b1b2 e4f5 b4c4 d7d8 d6c5 d8e8 b4b3 f8g9 b1b2 g9f8 b4b3 f5g6 c2d3 f8f7 b2c2 i8i7 b1a1b2 i7i8 a2b2a1 i8h8 b1b2 f5g6 b2c2 i8h7 d2c2 g5h6 f4e3 h6g5 d2c2 e6f6 a2b2 h6g6 d2c2 f8e7 a2b2 d6e7 d2c2 f5g6 a2b2 f8f7 d3d4 h7g6 d6c5 i8h7 b4c4 e6f6 e3e4 f4g4 d2e3 g4f4 e4e5 h6g6 e7d6 e9e8 b4c4 e6f6 d6c5 f4g4 b3c4 h6g6 b2b3 h7h6 d6c5 f7f6 c2d3 f4g5 d3e4 i7h7 d5e5 f7f6 a3b3 h7g7 b3c4 i5h4 g6g5 g3h4 f4g4 h6h5 g5f4 h4h5 c5d5 i8h8 g4f3 g5h6g6 b4c5 h5i5h6 d3e4 g8g7 g5f4 i6i7")
game2 = White <> (take 9 $ readGame (negateBoard starting'board'belgianDaisy) "a1b2 a5b5 i9h8 a4b4 i8h7 b5c5 h7g6 g5f4 c3d4d3 h4g4 h9g8 c5d5 h8g8 i6h5 f6f8e6 c4d59.g6f6 h6g6 a2b3 b6c6 e8f8 b4c5 b3b4 c6c5 b2c2 c4d5 c3c4 d5e6 h9h8 c5d5 d4d3 f7e6 h8g7 f4f5 g7f7 g4f3f4 f8e7 g8g7 b3c3 e3f4 b4b3 f6e6 d7e7 g5g6 f7f6 f4f5 f7g8f8 i5h5 g9f8 g6f6 c6b6b5 b4c5 f8g9e8 e6e7")
game3 = White <> (take 13 $ readGame (negateBoard starting'board'belgianDaisy) "i9h8 i5h5 i8h7 h5g5 a1b2 a5b5 a2b3 h4g4 h7g6 b4c5 g6f5 i6h6 g7g8f7 b6c6 b3c3 a4b5 c4d4 d7d6 f4e4 b5c6 f7f6 c6c5 c1d1 g4g5 f8f7 c4d5 f4f5 g5g6 b1c1 g6h7 b2c2 h4h5 h9g9 h5h6 h9g9 h7g7 c1d2 d6e6 g6g5 h6g6 d1e2 g6f6 d4e4 g7g8 f9e8 c3d4 e4f4 h8g8 d8c7 h4h5 c2d3 g9g8 c7c6 h5h6 c6b5 h6h7 b5b4 d7c6 g4f4 h7g7 b4b3 g7f6 g5g4 g8g7 b3b2 c5b4 b2c3b1 g7f7 c7b6 d7c6 b6a5 f8e7 e9f9 c6c5 c2d2 c5d5 d3e4 b5c5 d2d3 c5d5 b1c2 c4c3 c1d2 c2c3 g5g4 c3c4 f9g9 d4e5 d2c2 b4c4 g3g4 d6d5 f3e3 c4c3 f2e2 g6f6 c2d3 c3b2c4 c1d1 b3b4 g9h9 e8f8 d1e2 d6d5 d2e2 b4b5 h9h8 e5f6 h5g5 d4d5 a5a4 h8g7 g4h5 g7f6 a4a3 f6e5 d1e2 d6c5 a3a2 e5d4 a1b1 f8e7 b1c1 e6f7e5 e2e3 g6f6g7 c1d2 b2c3 d2e2 g7f7 a2b2 e5d4 a1b1 b2c3 b1c1 c3d4 c1d2 b4c4b3 f2g3 c3b3c4 g7g6 d3c3 g6f5 d6d5 g3f3 c3c4 i9i8 b4c5")
game4 = Black <> (take 10 $ readGame (negateBoard starting'board'belgianDaisy) "a1b2 i5h5 a2b3 a5b6 i9h8 i6h6 h8g7 h6g6 b3c4 c5c7d6 b1c2 b4b6c5 g8g7f8 g4g5f3 c2d3 h4h5 c3d4 h6g5 b2c2 a4b5 i8h8 c5d6 c4c5 c7c6 h9h8 c5d6 h7h6 g7h7 g9g8 h4i5 h8g8 e7d6 h5h6 h8g7 h7h8 g7h7 h8g8 i5h5 e8e7 b5b4 c2c3 b4b5 e4d3 h5g4 c2c3 c7b6 c5d5 h7g6 f8e7 d8e8 c4d4 h4h5 f6e6 b6b5 g8f8 b4c5b3 f8e7 h6g5 f7e6 a2b2 c4d4 h4h5 c6d6 e8f8 h7g7 f3g4 g7f7 f8g8 f4e4 g4f4 e7f8 f5g6 f8f7 f4g4 d5e5 i5h4 d3e4 i8h8 e6f6 i6h5 c5d6d5 h4g3 h6g5 h5h4 d4e4 g3f2 f6f5 h8g7 f4f3")
game5 = White <> (take 12 $ readGame (negateBoard starting'board'belgianDaisy) "i9h8 i6h6 i8h7 h5g5 h7g6 a5b5 h8g7 a4b4 a1b1 g5f4 g8g7 f4h4e3 b1c2 e3e4 a2b2 b6c6 g6f6 b6b5 b1c1 g3f3 f5g5f4 b3c4 c2d3 c6c5 d6e7 c5c4 d1d2 c4c3 g7g8 b4b5c4 d2d3 d6e6 e7f8 h6g5 f4e4 d7d6 g4f4 c5d6 d5d4 d6e6 h6h7 e5d5 e4e5 e7e6 h7g7 i5h4 b2b3 h4h5 h9g9 e5f6 g9f8 h5g5 h8g8 f3g4 f8e7 f5f6 g8f8 c1c2 d6c6 c2c3 d2d3 b4c5 d3d4 c3c4 c7b6 e3d3 f4e4 c6c5 b3c3 g7f6 c3d3 g6f6 c6b6b5 c5d6 a5b5 d6d7 g9g8 f8f7 e8f8 b4c4 f4e3 d8d7 b2c2 e6f7 h9h8 d6e7 g9h9 f7g8")