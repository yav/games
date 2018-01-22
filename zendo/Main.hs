import Graphics.Vty

import Zendo
import UI



main :: IO ()
main =
  do cfg <- standardIOConfig
     vty <- mkVty cfg
     g <- newGame prop
     let pic = picForImage (drawGame g)
     update vty pic
     e <- nextEvent vty
     shutdown vty
     print ("Last event was: " ++ show e)
     print $ goodExamples g
