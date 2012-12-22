module Handler.BoardRender where

import Import

import qualified Data.Text as T
import qualified Data.List.Split as S

import System.Directory
import Control.Monad

import Board
import CairoRender

getBoardRenderR :: Text -> Handler RepHtml
getBoardRenderR boardRepr = do
  let denseRepr = map read $ S.splitOn "," $ T.unpack boardRepr
      board = denseReprToBoard denseRepr
      filename = "/tmp/abaloney-renders." ++ (concatMap show denseRepr) ++ ".svg"
  b <- liftIO $ doesFileExist filename
  unless b (liftIO $ saveBoard board filename)
  sendFile typeSvg filename
