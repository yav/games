module UI where

import Graphics.Vty
import qualified Data.Map as Map

import Zendo


drawGame :: Game -> Image
drawGame g = horizCat (map drawGrid (goodExamples g))
             <->
             char defAttr ' '
             <->
             horizCat (map drawGrid (badExamples g))

drawGrid :: Example -> Image
drawGrid example = vertCat $ map row $ take maxRow [ 0 .. ]
  where
  row y = horizCat
            [ Map.findWithDefault blank (x,y) mp | x <- take maxCol [ 0 .. ] ]
          <|> char defAttr '|'

  blank = char defAttr '.'

  mp = Map.fromList [ (at s, drawShape s) | s <- example ]

drawShape :: Item -> Image
drawShape i = char col ch
  where
  col = defAttr `withForeColor` case color i of
                                  Red -> red
                                  Green -> green
                                  Blue -> blue
  ch =
    case shape i of
      Square    -> '■'
      Circle    -> '●'
      Triangle  -> '▲'


