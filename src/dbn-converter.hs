module Main where

import Data.Binary
import MinimalNN
import System.Environment
import System.FilePath
import System.Directory
import Text.Printf

main :: IO ()
main = mapM_ convertDBN =<< getArgs

convertDBN :: FilePath -> IO ()
convertDBN fn = do
  let fnOut = dropExtension fn <.> "bin"
  ex <- doesFileExist fnOut
  case ex of
    False -> do
      putStrLn $ printf "Converting: %s -> %s" fn fnOut
      net <- read `fmap` readFile fn
      encodeFile fnOut (net :: TNetwork)
    True -> do
      putStrLn $ printf "Converting: %s -> %s [skipped]" fn fnOut
