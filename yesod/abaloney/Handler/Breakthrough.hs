module Handler.Breakthrough where

import Import
import qualified Data.Text as T
import qualified Data.List.Split as S
import Data.List (intercalate)
import Text.Blaze.Html

import BreakthroughGame
import AgentGeneric
import GenericGame

getBreakthroughR :: Int -> Text -> Handler RepHtml
getBreakthroughR mctsRating reprTxt = do
  agMCTS <- liftIO $ (mkAgent mctsRating :: IO AgentMCTS)

  let bRepr = map read $ S.splitOn "," $ T.unpack $ reprTxt
      game :: Breakthrough
      game = fromRepr (6,6) bRepr
      possibleMoves'P1 = moves game P1
      possibleMoves'P2 = moves game P2

      gameRepr g = T.intercalate "," $ map (T.pack . show) $ toRepr $ (g :: Breakthrough)

      prettyPrintGame' g = preEscapedToHtml $ intercalate "<br>" (lines (prettyPrintGame g))

  agMoveP1 <- liftIO $ applyAgent agMCTS game P1
  agMoveP2 <- liftIO $ applyAgent agMCTS game P2
  
  defaultLayout $ do
    setTitle "So abaloney... breakthrough edition."
    $(widgetFile "breakthrough")

