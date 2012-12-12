-- dense CSV to sparse CSV

module Main where

import System.Environment
import System.IO
import Data.List (intercalate)

import System.FilePath

import Text.Printf

main = do
  files <- getArgs
  
  mapM_ workOnFile files

workOnFile fname = do
  let newName = (takeFileName fname) <.> "sparse" <.> (takeExtension fname)
  putStrLn (printf "Converting file: %s to %s" fname newName)
  
  input <- lines `fmap` readFile fname
  let output = map translateRow input
  writeFile newName (unlines output)

translateRow row = lineBack
    where
      values = read ("[" ++ row ++ "]") :: [Int]

      boolToDouble False = 0
      boolToDouble True = 1

      mkVector val = map boolToDouble $ map (==val) values

      vecEmpty = mkVector 0
      vecWhite = mkVector 1
      vecBlack = mkVector 2
      
      vecAll = vecEmpty ++ vecWhite ++ vecBlack

      lineBack = (intercalate "," (map show vecAll))
