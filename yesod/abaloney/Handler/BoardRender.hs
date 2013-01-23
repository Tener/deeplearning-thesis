module Handler.BoardRender where

import Import

import qualified Data.Text as T
import qualified Data.List.Split as S

import System.Directory
import System.Process
import Control.Monad

import Board
import CairoRender

getBoardRenderR :: Text -> Text -> Handler RepHtml
getBoardRenderR mode boardRepr = do
  let denseRepr = map read $ S.splitOn "," $ T.unpack boardRepr
      board = denseReprToBoard denseRepr
      filename'svg = "/tmp/abaloney-renders." ++ (concatMap show denseRepr) ++ ".svg"
      filename'jpg = "/tmp/abaloney-renders." ++ (concatMap show denseRepr) ++ ".jpg"
      filename'png = "/tmp/abaloney-renders." ++ (concatMap show denseRepr) ++ ".png"

      mk'svg = liftIO $ (saveBoard board filename'svg :: IO ())
      mk'jpg = mk'svg >> convert filename'svg filename'jpg >> return ()
      mk'png = mk'svg >> convert filename'svg filename'png >> return ()

      convert fin fout = liftIO (rawSystem "convert" ["-resize", "600x", fin,fout])

      (filename,mkFile,ftype) = case mode of
                            "svg" -> (filename'svg, mk'svg, typeSvg)
                            "png" -> (filename'png, mk'png, typePng)
                            "jpg" -> (filename'jpg, mk'jpg, typeJpeg)
                            _ -> error ("bad filetype: " ++ show mode)

  b <- liftIO $ doesFileExist filename
  unless b mkFile
  sendFile ftype filename
