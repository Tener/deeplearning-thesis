module Main where

import Data.Binary
import MinimalNN
import System.Environment
import System.FilePath
import Text.Printf

main :: IO ()
main = mapM_ convertDBN =<< getArgs

convertDBN :: FilePath -> IO ()
convertDBN fn = do
  let fnOut = dropExtension fn <.> "bin"
  putStrLn $ printf "Converting: %s -> %s" fn fnOut
  net <- read `fmap` readFile fn
  encodeFile fnOut (net :: TNetwork)
