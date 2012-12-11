{-# LANGUAGE OverloadedStrings #-}

module SVG where

import Board

import System.Process (rawSystem)

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "1500" ! A.height "1000" ! A.viewbox "0 0 3 2" $ do
    S.g ! A.transform makeTransform $ do
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#008d46"
      S.rect ! A.width "1" ! A.height "3" ! A.fill "#ffffff"
      S.rect ! A.width "1" ! A.height "4" ! A.fill "#d2232c"
      S.path ! A.d makePath

makePath :: S.AttributeValue
makePath = mkPath $ do
  S.lr 100 0
  S.lr 0 100
  S.lr (-100) 0
  S.lr 0 (-100)
  S.z


makeTransform :: S.AttributeValue
makeTransform = rotate 50


draw svg = do
  let fname = "/tmp/tmp.svg"
  writeFile fname (renderSvg svg)
  rawSystem "inkscape" [fname]

testDraw = do
  let a = renderSvg svgDoc
  putStrLn a
  draw svgDoc