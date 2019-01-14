module RenderText where

import Graphics.Vty
import Util.Bag
import qualified Data.Map as Map

import Game

main :: IO ()
main =
  do cfg <- standardIOConfig
     vty <- mkVty cfg
     loop vty newGame
     shutdown vty

loop :: Vty -> Game -> IO ()
loop vty g =
  do update vty (drawGame g)
     e <- nextEvent vty
     case handleEvent e g of
       Nothing -> pure ()
       Just g1 -> loop vty g1

norm :: Attr
norm = defAttr `withBackColor` black `withForeColor` white

inv :: Attr
inv = defAttr `withBackColor` white `withForeColor` black 

drawGame :: Game -> Picture
drawGame g = picForLayers $ map drawMeeple (Map.toList (meeples g)) ++
                            [ vertCat [ drawTerrain g
                                      , string norm " "
                                      , drawControls g
                                      ] ]

drawTerrain :: Game -> Image
drawTerrain g = vertCat [ horizCat [ string norm "." | _ <- range (width g) ]
                              | _ <- range (height g) ]
  where
  range n = take n [ 0 .. ]

drawControls :: Game -> Image
drawControls g = vertCat (stat : map lkp [ N, E, S, W ] ++
                            [ string norm ("Steps: " ++ show (steps g)) ])
  where
  cs    = controls g
  lkp d = drawPowers (Just d == bg) d (Map.findWithDefault bagEmpty d cs)
  (bg,stat) = case status g of
                   Distribute d xs ->
                     (Just d, horizCat ( string norm "Distribute: "
                                       : map drawPower (bagToListGrouped xs)))
                   Activate -> (Nothing, string norm "Chose direction:")
                   Finished -> (Nothing, string norm "Game finished")
  sp    = string norm " "

drawMeeple :: (MeepType,Loc) -> Image
drawMeeple (t,(x,y)) = translate x y (char attr 'M')
  where
  attr = withForeColor norm (meepColor t)

drawPower :: (MeepType,Int) -> Image
drawPower (t,n) = horizCat (replicate n (char attr '*'))
  where attr = withForeColor norm (meepColor t)

meepColor :: MeepType -> Color
meepColor (MeepType t) = ISOColor (fromIntegral t)

drawPowers :: Bool -> Dir -> Bag MeepType -> Image
drawPowers hi d ps =
  horizCat ( string attr (show d ++ ": ")
          : map drawPower (bagToListGrouped ps)
          )
  where
  attr = if hi then inv else norm


handleEvent :: Event -> Game -> Maybe Game
handleEvent ev g =
  case ev of
    EvKey k _ ->
      case k of
        KChar 'q' -> Nothing
        KChar 'r' -> Just (distribute (MeepType 1) g)
        KChar 'y' -> Just (distribute (MeepType 3) g)
        KChar 'n' -> Just (activate N g)
        KChar 'e' -> Just (activate E g)
        KChar 's' -> Just (activate S g)
        KChar 'w' -> Just (activate W g)
        _ -> Just g
    _ -> Just g



