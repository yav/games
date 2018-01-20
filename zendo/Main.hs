import Graphics.Vty

import Zendo
import UI

prop = Count "x" (ShapeOf (Var "x") :=: Shape Circle) :>=:
       Count "x" (ShapeOf (Var "x") :=: Shape Square)
  -- Exists "x" $ Exists "y" $ Touching (Var "x") (Var "y")


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
