module Test where

import Board
import CairoRender

import qualified Math.Geometry.GridMap as GridMap
import Data.List (group, sort, nubBy, nub)

main = do

  let testOne (i,(idx, way, rule, brd)) = do
         print (idx, way)
         print rule
         CairoRender.draw brd i

      proj (a,b,c,d) = d
      mynub = nubBy (\ brd0 brd1 -> (proj brd0) == (proj brd1))

  mapM_ testOne (zip [0..] (mynub $ Board.test White starting'board'death))
