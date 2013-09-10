-- general utilities not really connected with the main focus of this project

module Utils where

import Text.Printf
import System.Random.MWC
import Data.List (sortBy, group, nub, nubBy, sort)
import Data.Ord (comparing)
import Control.Monad (replicateM)


data Hidden a = Hidden { unhide :: a }

class TypeTag a where
    getTypeTag :: a -> String -- get 'name' of a type

instance (TypeTag a) => Show (Hidden a) where
    show (Hidden foo) = (printf "Hidden {<%s>}" (getTypeTag foo) :: String)

instance Eq (Hidden a) where
    _ == _ = True

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
