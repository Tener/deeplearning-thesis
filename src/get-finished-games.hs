module Main where

-- filter files to get only finished games. reads dense files, outputs sparse ones.

import System.Environment
import System.IO
import Data.List (intercalate)

import System.FilePath

import Text.Printf
import Data.Maybe

import Board

main = do
  files <- getArgs
  mapM_ workOnFile files

workOnFile fname = do
  let newName = (takeFileName fname) <.> "won-sparse" <.> (takeExtension fname)
  putStrLn (printf "Converting file: %s to %s" fname newName)
  
  input <- lines `fmap` readFile fname
  let output = catMaybes $ map translateRow input
  if (length output) == 0 
   then putStrLn "Skipping empty output file."
   else writeFile newName (unlines output)

translateRow input'row = if isFinished brd then Just row else Nothing
    where
      values = read ("[" ++ input'row ++ "]") :: [Int]
      brd = denseReprToBoard values
      row = reprToRow $ boardToSparse brd
      