{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances #-}

module MyVectorType (module MyVectorType
#ifdef VECTOR_HMATRIX
 , Matrix.toLists, Matrix.fromLists
 , Matrix.trans
 , Vector.toList, Vector.fromList
 , Vector.dim
 , Numeric.Container.vXm, Numeric.Container.sumElements
#endif
)
where


#ifdef VECTOR_HMATRIX
import qualified Data.Packed.Vector as Vector
import qualified Data.Packed.Matrix as Matrix 
import qualified Numeric.Container
import Numeric.Container (Element, Container)
import Numeric.LinearAlgebra () -- instances
type Vector = Vector.Vector
type Matrix = Matrix.Matrix
instance Ord (Matrix Double) where
    compare a b = compare (Matrix.toLists a) (Matrix.toLists b)
cmap :: (Element b, Container c e) => (e -> b) -> c e -> c b
cmap a  = Numeric.Container.cmap a

add :: Container c e => c e -> c e -> c e
add a = Numeric.Container.add a

transFix :: Matrix Double -> Matrix Double
transFix = id

concat :: (Storable a) => [Vector a] -> Vector a
concat = Vector.join
#endif

#ifdef VECTOR_VECTOR
import Data.Vector.Binary () -- instances
import Data.List (transpose)
#ifdef VECTOR_DDOT
import Foreign.C.Types
import Foreign
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Internal as VI
#else
import qualified Data.Vector as V
#endif

type Vector a = V.Vector a
type Matrix a = [Vector a]

#ifdef VECTOR_DDOT
foreign import ccall "cblas_ddot" cblas_ddot :: CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble
vDOTv :: Vector Double -> Vector Double -> Double
vDOTv v1 v2 = let (fptr'1,len1) = V.unsafeToForeignPtr0 (V.unsafeCast v1)
                  (fptr'2,len2) = V.unsafeToForeignPtr0 (V.unsafeCast v2)
                  ptr'v1 = VI.getPtr fptr'1
                  ptr'v2 = VI.getPtr fptr'2
                  r = (cblas_ddot (fromIntegral len1) ptr'v1 1 ptr'v2 1)
              in realToFrac r
vXm :: Vector Double -> Matrix Double -> Vector Double
vXm v m = V.fromList $ map (vDOTv v) m
#else
vXm :: Vector Double -> Matrix Double -> Vector Double
vXm v m = V.fromList $ map (\ vm -> V.sum $ V.zipWith (*) v vm) m
#endif

add :: Vector Double -> Vector Double -> Vector Double
add = V.zipWith (+)

cmap :: (Double -> Double) -> Vector Double -> Vector Double
cmap = V.map

toLists :: Matrix Double -> [[Double]]
toLists = map V.toList
fromLists :: [[Double]] -> Matrix Double
fromLists = map V.fromList
trans :: Matrix Double -> Matrix Double
trans = fromLists . transpose . toLists

transFix :: Matrix Double -> Matrix Double
transFix = trans

toList :: Vector Double -> [Double]
toList = V.toList
fromList :: [Double] -> Vector Double
fromList = V.fromList

sumElements :: Vector Double -> Double
sumElements = V.sum

dim :: Vector Double -> Int
dim = V.length

concat :: [Vector a] -> Vector a
concat = V.concat
#endif

-- #ifdef VECTOR_DPH
-- import qualified Prelude
-- import Prelude ((.), ($))
-- -- import Data.Array.Parallel
-- -- import Data.Array.Parallel.Prelude
-- import Data.Array.Parallel.Prelude.Double
-- import Data.Array.Parallel.Prelude.Int (Int)
-- import Data.Array.Parallel.Unlifted as V
-- import Data.List (transpose)
--  
-- type Vector a = V.Array a
-- type Matrix a = [Vector a]
--  
-- vXm :: Vector Double -> Matrix Double -> Vector Double
-- vXm v m = Prelude.map (\ vm -> V.sum $ V.zipWith (*) v vm) m
--  
-- add :: Vector Double -> Vector Double -> Vector Double
-- add = V.zipWith (+)
--  
-- cmap :: (Double -> Double) -> Vector Double -> Vector Double
-- cmap = V.map
--  
-- toLists :: Matrix Double -> [[Double]]
-- toLists = V.toList . V.map V.toList
-- fromLists :: [[Double]] -> Matrix Double
-- fromLists = V.map V.fromList . V.fromList
-- trans :: Matrix Double -> Matrix Double
-- trans = fromLists . transpose . toLists
--  
-- transFix :: Matrix Double -> Matrix Double
-- transFix = trans
--  
-- toList :: Vector Double -> [Double]
-- toList = V.toList
-- fromList :: [Double] -> Vector Double
-- fromList = V.fromList
--  
-- sumElements :: Vector Double -> Double
-- sumElements = V.sum
--  
-- dim :: Vector Double -> Int
-- dim = V.length
--  
-- #endif



