-- general utilities not really connected with the main focus of this project

module Utils where

import Text.Printf
import System.Random.MWC
import Data.List (sortBy)
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
shuffle lst = withSystemRandom . asGenST $ \ gen -> do
  ixs <- replicateM (length lst) (uniform gen)
  return $ map snd $ sortBy (comparing fst) $ (zip (ixs::[Int]) lst)
  
ofType :: a -> a -> a
ofType a _ = a
