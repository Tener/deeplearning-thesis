{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- parsing files from www.LittleGolem.net

module LittleGolem where

import BreakthroughGame
import GenericGame

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text.IO 

data GameRecord a = GameRecord { event, site, playerWhite, playerBlack, result :: Text
                               , moveSequence :: [a]
                               } deriving Show

data BrMove = Resign | Move Position Position

class LittleGolemMoveParser a where
    parseMoveSequence :: Parser [a]

instance LittleGolemMoveParser Breakthrough where
    parseMoveSequence = do
      let movePair :: Parser [(MoveDesc Breakthrough)]
          movePair = do
            let moveOrResign = (pure [] <* string "resign ") <|> (fmap ((:[]) . brMoveToDesc) move <* char ' ')
                pTwice = do
                          m1 <- moveOrResign
                          m2 <- moveOrResign
                          return [m1,m2]
                pOnce = do
                          m1 <- moveOrResign
                          return [m1]
            counter
            fmap concat (pTwice <|> pOnce)
                         
          counter = takeWhile1 (/= '.') >> char '.' >> space -- counter, "34. "
          pos :: Parser Position
          pos = do
            row' <- anyChar
            col' <- anyChar
            return (positionLiteralToPos row' col')

          brMoveToDesc Resign = ((0,0),(0,0))
          brMoveToDesc (Move p1 p2) = (p1,p2)

          move :: Parser BrMove
          move = (pure Resign <* string "resign")
                  <|> (do
                        p1 <- pos 
                        (string "-" <|> string "x")
                        p2 <- pos
                        return (Move p1 p2)
                      )
          positionLiteralToPos row' col' = (colLit row', rowLit col')

          rowLit '1' = 0
          rowLit '2' = 1
          rowLit '3' = 2
          rowLit '4' = 3
          rowLit '5' = 4
          rowLit '6' = 5
          rowLit '7' = 6
          rowLit '8' = 7
          rowLit _ = error "invalid row"

          colLit 'a' = 0
          colLit 'b' = 1
          colLit 'c' = 2
          colLit 'd' = 3
          colLit 'e' = 4
          colLit 'f' = 5
          colLit 'g' = 6
          colLit 'h' = 7
          colLit _ = error "invalid column"


      movePairs <- many movePair
      _victory <- string "0-1" <|> string "1-0" <|> string "*"

      let g0 = freshGame (8,8) :: Breakthrough
          mvs = concat movePairs
          applyMove' g m = case applyMove g m of
                             Nothing -> error "invalid move"
                             Just g' -> g'
          all'games = scanl (applyMove') g0 mvs

      return all'games
            
parseGameFileName :: (LittleGolemMoveParser a) => FilePath -> IO [GameRecord a]
parseGameFileName fn = do
  res <- parseOnly parseGameFile `fmap` Data.Text.IO.readFile fn
  case res of
     Left err -> print err >> return []
     Right res' -> return res'

tagged :: Text -> Parser Text
tagged tag = do
  char '['
  string tag
  space
  char '"'
  cont <- takeWhile1 (/= '"') <?> "contents"
  char '"'
  char ']'
  takeWhile1 (/= '\n') <?> "get till eol"
  endOfLine
  return cont

parseGameFile :: (LittleGolemMoveParser a) => Parser [GameRecord a]
parseGameFile = many parseGameRecord

line :: Parser Text
line = takeWhile1 (/='\n') <* char '\n'

parseGameRecord :: (LittleGolemMoveParser a) => Parser (GameRecord a)
parseGameRecord = do
  f0 <- tagged "Event"      <?> "event" 
  f1 <- tagged "Site"       <?> "site" 
  f2 <- tagged "White"      <?> "white" 
  f3 <- tagged "Black"      <?> "black" 
  f4 <- tagged "Result"     <?> "result" 
  f5 <- parseMoveSequence <* char '\n'
  char '\n'
  return (GameRecord f0 f1 f2 f3 f4 f5) <?> "GameRecord"
