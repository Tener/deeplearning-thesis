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
          ll = swapTuple ([0.8612870072635965,0.13556289898576446,0.23856740534874787,-0.9979329974235303,0.5214206483083219,0.5567218465572636,6.30721199599138e-2,-0.9964997583716422,0.6633215227291445,0.9727764072367318,-0.9977300656726051,-0.7137626797569403,0.599503073952065,0.21117238019021678,-0.8943134998344482,-0.8507363319229861,-0.5796927500773281,-0.5944415874686173,-0.15455666225322306,0.48421792303516864,7.27231106402837e-2,-0.9283644693418183,0.30689324517669125,-0.3539762275595806,-0.6070475938654247,0.569425951578671,-0.9770268090296865,-0.7042885676466255,0.829879962337911,-0.8374036555090278,0.4084455058795846,0.19033645119509512,0.5226687071932188,0.37593093252835463,0.8750966180591477,0.2783969819593024,0.21502082061293115,0.5328102176833449,-0.913095686465424,1.7834153344699955e-2,-0.21090731225389203,-0.7926096658144519,0.33027529205870665,-0.5950117361138936,-0.2322369595737246,0.8544967675730344,0.8832103002110319,-0.48539113861681993,-0.8998826600710117,-0.47158017053846035,0.706039676126762,-0.28561829110183434,0.9198783114683449,0.3895434736047527,-0.2832766779528131,-0.20329351024642128,4.417816111277628e-2,-0.19855374724845487,-8.127954092267964e-2,0.8928326986485555,-0.7989752224829971,-0.33452644307129176,-6.750250789346235e-2,-6.010680309923688e-2,0.4154639143443546,0.9648585837921593,0.9480414301364253,-0.6649152911500036,0.6882932372250472,0.7699115131259664,0.3884008422743015,0.6881782900708593,-5.634221277961582e-2,-0.715489425131538,-0.8616836802575512,0.6833222561027494,0.31588681243071215,-0.38215905000508044,0.43148713223178947,-0.4126939727483758,-0.8082677460400503,0.9729490146612128,0.6163305835165447,-0.3789216454834452,-0.6158593001591457,5.109623424964349e-3,-4.076986403769234e-2,-0.2314156693706757,0.7605746771119992,-0.7303217846056131,0.5799821294014398,0.74224702249001,0.31361764991484775,-0.9989099173730651,0.3990524857254252,-0.32232293073842144,-0.9131409321750923,0.15206208775660257,0.6079320679483644,0.5941176532464534],0.0)
--          [ll] = lastLayerTN $ fst myUnsafeNetLL
--          ll' = (0, replicate (last sizes) 1)

      return (AgentNN neuralNetwork ll colo)
    makeMove agent brd = do
      let gst = GameState brd (\ g -> doubleToEvalInt $ evalBoardNet (gtColorNow g) (gtBoard g) (net agent) (lastLayer agent)) 
                              (col agent) (col agent)
          depth = 1
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

