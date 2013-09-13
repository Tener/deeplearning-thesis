-- general utilities not really connected with the main focus of this project

module Utils where

import Text.Printf
import System.Random.MWC
import Control.Monad
import Data.List (sortBy, group, nub, nubBy, sort, transpose)
import Data.List.Split (splitEvery)
import Data.Ord (comparing)
import Control.Monad (replicateM)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Applicative ((<$>))
import System.ProgressBar as Bar
import System.IO

data Hidden a = Hidden { unhide :: a }

class TypeTag a where
    getTypeTag :: a -> String -- get 'name' of a type

instance (TypeTag a) => Show (Hidden a) where
    show (Hidden foo) = (printf "Hidden {<%s>}" (getTypeTag foo) :: String)

instance Eq (Hidden a) where
    _ == _ = True

splitWorkThreads :: [a] -> IO [[a]]
splitWorkThreads lst = do
  caps <- getNumCapabilities
  return (splitWorkCaps caps lst)

runWorkThreadsProgress_ :: [IO a] -> IO ()
runWorkThreadsProgress_ acts = do
  hSetBuffering stdout NoBuffering
  progVar <- newMVar 0
  let total = fromIntegral $ length acts
      fract = max 1 (total `div` 100)
      incProgVar = modifyMVar_ progVar (return . (+1))
      dispProg = withMVar progVar $ \ progress -> do
        when (progress `mod` fract == 0) (progressBar (Bar.msg "runWorkThreads_::jobProgress") Bar.exact 120 progress total)
      acts' = map (\a -> incProgVar >> dispProg >> a) acts
  jobLists <- splitWorkThreads acts'
  print ("runWorkThreads_::jobLists", length jobLists, map length jobLists)
  mapConcurrently sequence_ jobLists
  putStrLn ""
  print ("runWorkThreads_::done")  
  return ()

runWorkThreads_ :: [IO a] -> IO ()
runWorkThreads_ acts = void (mapConcurrently sequence_ =<< splitWorkThreads acts)

runWorkThreads :: [IO a] -> IO [a]
runWorkThreads acts = concat <$> (mapConcurrently sequence =<< splitWorkThreads acts)

splitWorkCaps :: Int -> [a] -> [[a]]
splitWorkCaps caps lst = transpose $ splitEvery caps $ lst

fixNaN x | isNaN x = 0
         | otherwise = x

shuffle :: [a] -> IO [a]
shuffle lst = mkGenIO >>= \ gen -> do
  ixs <- replicateM (length lst) (uniform gen)
  return $ map snd $ sortBy (comparing fst) $ (zip (ixs::[Int]) lst)

shuffleGen :: GenIO -> [a] -> IO [a]
shuffleGen gen lst = do
  ixs <- replicateM (length lst) (uniform gen)
  return $ map snd $ sortBy (comparing fst) $ (zip (ixs::[Int]) lst)

ofType :: a -> a -> a
ofType a _ = a

nubSort :: (Ord el) => [el] -> [el]
nubSort xs = map head $ group $ sort xs

boolToMaybe :: Bool -> x -> Maybe x
boolToMaybe False _ = Nothing
boolToMaybe True x = Just x

mkGenIO :: IO GenIO
mkGenIO = withSystemRandom $ asGenIO $ return

pickList :: GenIO -> [a] -> IO a
pickList rgen xs = do
          pick <- uniformR (0, (length xs) - 1) rgen
          return (xs !! pick)

pickListWeighted :: GenIO -> [(Double, a)] -> IO a
pickListWeighted rgen xs = do
          let wSum = sum (map fst xs)
              xsCum = scanl1 (\ (w0,_x0) (w1,x1) -> (w0+w1, x1)) xs
          p <- uniformR (0, wSum) rgen
          return $ snd $ head $ dropWhile (\(w,_) -> w < p) xsCum
