{-# LANGUAGE CPP, BangPatterns, ViewPatterns #-}

module NeuralNets where

import Control.Monad(when, join)
import qualified Data.Tree.Game_tree.Negascout as GTreeAlgo
import Text.Printf
import Data.List.Split(splitPlaces)
import qualified Numeric.Container as NC
import qualified Data.Packed.Vector as Vector


import System.IO.Memoize (ioMemo')
import System.IO.Unsafe
import System.IO

import Board
import CommonDatatypes
import MinimalNN

type LastLayer = (Double,[Double])

data AgentNN = AgentNN { net :: TNetwork
                       , lastLayer :: LastLayer
                       , col :: Color
                       } deriving (Show, Ord, Eq)

data AgentNNSimple = AgentNNSimple { netS :: TNetwork
                                   , colS :: Color
                                   } deriving (Show, Ord, Eq)

data AgentNNSimpleLL = AgentNNSimpleLL { netSLL :: TNetwork
                                       , colSLL :: Color
                                       } deriving (Show, Ord, Eq)

  
-- [3] SCORE 1.0
-- [3] NEURON ([[[-0.5722650158768026,-4.4325037942309686e-2,-0.43404105854964015,0.10689952619295817,0.577792557221745,9.471280627105672e-2,-3.355368050165031e-2,4.6718569207590877e-2,-0.5434123062190859,0.2484268351141663,0.7960228565299461,0.6128597704944225,-9.574998923713918e-2,-0.5554837837947575,-0.8362088117682871,-0.1249442511845873,0.8010136546408504,0.3989169781535946,0.25555669139626036,-0.4980766236043739,0.2407154519880168,-0.4425795194250426,-0.37372410774232834,0.7032841059489596,0.797465694138545,-0.9374286002987204,-0.5542879786103809,-0.7027563485976887,-0.564229209189475,-0.14396630481766537,0.9660832414778444,0.6759396748887994,0.1355553736496169,-0.6779103447331825,-0.38605527106568327,0.5003390061167357,-0.7214014746854327,-0.854003911577436,-0.8200696665688179,-0.6948381791633225,-0.556167126958887,-0.17876488293740045,-0.22298238314689578,0.7992281446807132,0.5568577041149256,-0.9907674764170122,4.426502619935113e-2,-0.9695348825638723,-0.741853642085929,-0.42918501552202915,-0.45172937250195244,0.9185330336744799,-0.3160250025793496,0.23499908198436104,5.1803251803267836e-2,0.3026470441581528,0.48693006090825297,0.27378680257457355,0.20362811552041093,0.8707005914056747,-0.7357970882818876,-0.40761642340025506,-0.23735300452820263,0.44694649375740947,-7.533402975962411e-2,-0.46328382547467295,-0.11390515110701283,-0.972891852275424,-0.507725014792556,0.8747299548404437,-0.9872274093759683,-0.8135082616149185,0.32672616111773434,0.9462363821427926,-5.452161398015254e-2,-0.4086905188361516,0.77619001959468,0.3671761659363695,0.5858326919173653,-0.7703574693462534,-0.14548005115326235,-0.2397249282343814,-0.6446746554663949,-0.9595178327110767,0.8023722988209763,2.997380416946971e-4,1.3192549092589045e-2,7.220073302479935e-2,-5.574508706288173e-3,-0.20490220770001888,-0.41415205022124035,0.5684775093941024,0.35790945393055495,0.9284802193762489,-2.505878925857341e-2,-0.48711111823000297,-0.8732610051414083,0.2885757350289706,0.6629953209185453,0.6013582083759121]]],[[0.0]])


instance Agent AgentNNSimple where
    mkAgent colo = do
      let (!neuralNetwork, sizes) = myUnsafeNet
      print ("Agent created","AgentNNSimple")
      return (AgentNNSimple neuralNetwork colo)
    makeMove ag@(AgentNNSimple neuralNetwork colo) brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNetOnePassN 1 (gtColorNow g) (gtBoard g) neuralNetwork)
                          colo colo
          depth = 3
          (princ, score) = GTreeAlgo.negascout gst depth
      print ("AgentNNSimple", score, (take 3 $ evaluateBoard ag brd))
      return (gtBoard $ head $ tail $ princ)
    evaluateBoard (AgentNNSimple neuralNetwork colo) brd = 
        [("int" <> valInt)
        ,("bare" <> valDbl)
        ,("network",valNet)]
     where
       valInt = doubleToEvalInt $ valDbl
       valDbl = evalBoardNetOnePassN 1 colo brd neuralNetwork
       brdNeg = if colo == White then brd else negateBoard brd
       valNet = unwords $ map (printf "%0.2f") $ Vector.toList $ computeTNetworkSigmoidSteps 1 neuralNetwork (boardToSparseNN brdNeg)
       s <> v = (s, (show v))
                

instance Agent AgentNNSimpleLL where
    mkAgent colo = do
      let (!neuralNetwork, sizes) = myUnsafeNetLL
      return (AgentNNSimpleLL neuralNetwork colo)
    makeMove (AgentNNSimpleLL neuralNetwork colo) brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNetOnePass (gtColorNow g) (gtBoard g) neuralNetwork)
                          colo colo
          depth = 3
          (princ, score) = GTreeAlgo.negascout gst depth
      print ("AgentNNSimpleLL", score)
      return (gtBoard $ head $ tail $ princ)

instance Agent AgentNN where
    mkAgent colo = do
      let (!neuralNetwork, sizes) = myUnsafeNet
          swapTuple (a,b) = (b,a)
          ll :: LastLayer
          ll = swapTuple ([-0.5722650158768026,-4.4325037942309686e-2,-0.43404105854964015,0.10689952619295817,0.577792557221745,9.471280627105672e-2,-3.355368050165031e-2,4.6718569207590877e-2,-0.5434123062190859,0.2484268351141663,0.7960228565299461,0.6128597704944225,-9.574998923713918e-2,-0.5554837837947575,-0.8362088117682871,-0.1249442511845873,0.8010136546408504,0.3989169781535946,0.25555669139626036,-0.4980766236043739,0.2407154519880168,-0.4425795194250426,-0.37372410774232834,0.7032841059489596,0.797465694138545,-0.9374286002987204,-0.5542879786103809,-0.7027563485976887,-0.564229209189475,-0.14396630481766537,0.9660832414778444,0.6759396748887994,0.1355553736496169,-0.6779103447331825,-0.38605527106568327,0.5003390061167357,-0.7214014746854327,-0.854003911577436,-0.8200696665688179,-0.6948381791633225,-0.556167126958887,-0.17876488293740045,-0.22298238314689578,0.7992281446807132,0.5568577041149256,-0.9907674764170122,4.426502619935113e-2,-0.9695348825638723,-0.741853642085929,-0.42918501552202915,-0.45172937250195244,0.9185330336744799,-0.3160250025793496,0.23499908198436104,5.1803251803267836e-2,0.3026470441581528,0.48693006090825297,0.27378680257457355,0.20362811552041093,0.8707005914056747,-0.7357970882818876,-0.40761642340025506,-0.23735300452820263,0.44694649375740947,-7.533402975962411e-2,-0.46328382547467295,-0.11390515110701283,-0.972891852275424,-0.507725014792556,0.8747299548404437,-0.9872274093759683,-0.8135082616149185,0.32672616111773434,0.9462363821427926,-5.452161398015254e-2,-0.4086905188361516,0.77619001959468,0.3671761659363695,0.5858326919173653,-0.7703574693462534,-0.14548005115326235,-0.2397249282343814,-0.6446746554663949,-0.9595178327110767,0.8023722988209763,2.997380416946971e-4,1.3192549092589045e-2,7.220073302479935e-2,-5.574508706288173e-3,-0.20490220770001888,-0.41415205022124035,0.5684775093941024,0.35790945393055495,0.9284802193762489,-2.505878925857341e-2,-0.48711111823000297,-0.8732610051414083,0.2885757350289706,0.6629953209185453,0.6013582083759121],0.0)
--          [ll] = lastLayerTN $ fst myUnsafeNetLL
--          ll' = (0, replicate (last sizes) 1)

      return (AgentNN neuralNetwork ll colo)
    makeMove agent brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNet (gtColorNow g) (gtBoard g) (net agent) (lastLayer agent)) 
                              (col agent) (col agent)
          depth = 5
          (princ, score) = GTreeAlgo.negascout gst depth

          pr = "http://localhost:3000/board/" ++ (reprToRow $ boardToDense $ gtBoard $ last $ princ)
      print ("AgentNN-score",score)
      hPutStrLn stderr ("AgentNN-score " ++ show score ++ " " ++ pr)
      return (gtBoard $ head $ tail $ princ)

printE :: (Show a) => a -> IO ()
printE p = do
  hPutStr stderr (show p)
  hPutStr stderr "\n"

doubleToEvalInt :: Double -> Int
doubleToEvalInt d = round (d * (fromIntegral ((maxBound :: Int) `div`10)))

parseNetFromFile'' fp = ioMemo' (parseNetFromFile `fmap` readFile fp)
parseNetFromFile' = join $ parseNetFromFile'' "/home/tener/abalone-nn/nn_183.txt-100"
parseNetFromFile'LL = join $ parseNetFromFile'' "nn_ll.txt"

{-# NOINLINE myUnsafeNet #-}
myUnsafeNet = unsafePerformIO parseNetFromFile'

{-# NOINLINE myUnsafeNetLL #-}
myUnsafeNetLL = unsafePerformIO parseNetFromFile'LL

parseNetFromFile :: String -> (TNetwork, [Int])
parseNetFromFile input = asserts $ (result, sizes) -- (length weights, length weights'split, length biases, neuronCount, layerCount, sizes)
  where input'lines@(sizes'row : rest'rows) = lines input
        sizes = readIntsRow sizes'row -- sizes in first line

        neuronCount = sum sizes
        layerCount = length sizes

        rest'double'rows :: [[Double]]
        rest'double'rows = map readDoublesRow rest'rows

        weights, biases :: [[Double]]
        (biases,moar) = splitAt layerCount rest'double'rows -- biases in next layerCount lines -- in each appropriate number of biases
        (weights,garbage) = splitAt neuronCount $ moar -- weights in next neuronCount lines
        weights'split = splitPlaces sizes weights

        --- weights, biases, neuronCount, layerCount, sizes
        -- biases'neg = (map (map negate) biases)  -- must negate biases -- different from matlab -- not any more

        result = network
        network = mkTNetwork weights'split biases

        asserts r | garbage /= [] = error (printf "parseNetFromFile: garbage not empty: %d elements" (length garbage))
                  | length weights /= neuronCount = error (printf "parseNetFromFile: too little weights: %d (should be %d)" (length weights) neuronCount)
                  | length weights'split /= layerCount = error "parseNetFromFile: not enough weights?"
                  | otherwise = r -- r for result


readDoublesRow :: String -> [Double]
readDoublesRow row = map read (words row)

readIntsRow :: String -> [Int]
readIntsRow row = map read (words row)

evalBoardNet :: Color -> Board -> TNetwork -> LastLayer -> Double
evalBoardNet col brd net (ll'b, ll'w) = result
    where
      brdEval = if col == White then brd else negateBoard brd
      values = boardToSparseNN brdEval

      net'll :: TNetwork
      net'll = mkTNetwork [[ll'w]] [[ll'b]]

      result'p1 = computeTNetworkSigmoid net values
      result'p2 = computeTNetworkSigmoid net'll result'p1

      combine = NC.sumElements
      result = combine result'p2

evalBoardNetOnePass :: Color -> Board -> TNetwork -> Double
evalBoardNetOnePass col brd net = result
    where
      brdEval = if col == White then brd else negateBoard brd
      values = boardToSparseNN brdEval
      result = NC.sumElements $ computeTNetworkSigmoid net values

evalBoardNetOnePassN :: Int -> Color -> Board -> TNetwork -> Double
evalBoardNetOnePassN steps col brd net = result
    where
      brdEval = if col == White then brd else negateBoard brd
      values = boardToSparseNN brdEval
      result = NC.sumElements $ computeTNetworkSigmoidSteps steps net values
     
g0 :: (Num a) => [a]
g0 = [1,0,1,0,0,0,0,0,1,1,1,1,0,1,1,0,1,1,1,0,1,1,1,0,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,0,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0]

