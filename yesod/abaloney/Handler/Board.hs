module Handler.Board where

import Import
import qualified Data.Text as T
import qualified Data.List.Split as S

import Board
import NeuralNets
import CommonDatatypes

getBoardR :: Text -> Handler RepHtml
getBoardR boardRepr = do
  ag'white <- liftIO $ mkAgent White
  ag'black <- liftIO $ mkAgent Black

  let denseRepr = map read $ S.splitOn "," $ T.unpack $ boardRepr
      board = denseReprToBoard denseRepr

      moves'white = getMoves White board
      moves'black = getMoves Black board

--      moves'white'repr = map boardToLink moves'white
--      moves'black'repr = map boardToLink moves'black

      boardToLink = T.pack . reprToRow . boardToDense

      eval b c = evaluateBoard (if c == White then ag'white :: AgentNNSimple else ag'black :: AgentNNSimple) b
      

  defaultLayout $ do
    setTitle "So abaloney..."
    $(widgetFile "board")

