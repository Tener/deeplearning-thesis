 {-# LANGUAGE DeriveDataTypeable #-}
module Config where

import Data.Typeable (Typeable)
import Data.Default
import Data.Global.Config
import Data.Function (fix)
import System.FilePath
import Control.Monad.IO.Class

data Config = Config { configNeuralNetsDirectory :: FilePath
                     , configNeuralNetworkSparse :: FilePath
                     , configNeuralNetworkDense :: FilePath
                     , configUseSparseRepr :: Bool
                     }
   deriving (Show, Typeable)

fetchConfig :: (Config -> a) -> IO a
fetchConfig field = fmap field (getConfig :: IO Config)

instance Default Config where
   def = fix (\self -> let d = configNeuralNetsDirectory self in 
                       Config "data-nn" (d </> "nn_183.txt-500") (d </> "nn_61.txt-100") False)

instance GlobalConfig Config where
   onSetConfig = liftIO . print

