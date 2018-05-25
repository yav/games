{-# Language OverloadedStrings #-}
import Control.Concurrent(threadDelay)
import SDL
import qualified SDL.Image as Image
import Control.Monad
import Data.List
import Foreign.C.Types(CInt)

main :: IO ()
main =
  do initializeAll
     Image.initialize [Image.InitPNG]
     w <- createWindow "My App" defaultWindow
                                  { windowResizable = True }
     r <- createRenderer w (-1) defaultRenderer
     box   <- staticObject <$> loadImg r "tiles/Floor1.png"
     mush  <- forwardBackObject <$> loadFrames r "Boiler1" 15
     smoke <- forwardObject <$> loadFrames r "BoilerSmoke" 30
     tree  <- forwardBackObject <$> loadFrames r "AnimTree1" 60
     wheel <- forwardObject . slowDown 2 <$> loadFrames r "AnimBWheel2" 90
     let frames = [box,mush,smoke,tree,wheel]
     appLoop frames r
     destroyRenderer r
     destroyWindow w
     Image.quit

staticObject :: Thing -> Object
staticObject = repeat

forwardObject :: [Thing] -> Object
forwardObject = cycle

forwardBackObject :: [Thing] -> Object
forwardBackObject xs = cycle (tail xs ++ tail (reverse xs))

slowDown n = concatMap (replicate n)

loadFrames :: Renderer -> String -> Int -> IO [Thing]
loadFrames r baseName frameNum =
            mapM (loadImg r . name baseName) (take frameNum [ 1 .. ])
  where
  index x = let xs = show x
            in replicate (4 - length xs) '0' ++ xs

  name x y = "tiles/" ++ x ++ "/" ++ x ++ index y ++ ".png"

tileSize :: Num a => a
tileSize = 256

loadImg :: Renderer -> FilePath -> IO Thing
loadImg r file =
  do surf     <- Image.load file
     V2 w h   <- surfaceDimensions surf
     texture  <- createTextureFromSurface r surf
     let hi = round ((tileSize
                          / fromIntegral w) * fromIntegral h :: Double)
     return (hi,texture)

type Point3d = (Float,Float,Float)

-- grid to pixel coords
coord :: Point3d -> (CInt,CInt)
coord (x,y,z) = (round (x1 * x + y1 * y + z1 * z)
               , round (x2 * x + y2 * y + z2 * z)
               )
  where
  x1 = 0.58 * tileSize
  y1 = -0.4 * tileSize

  x2 = 0.18 * tileSize
  y2 = 0.37 * tileSize

  z1 = 0 * tileSize
  z2 = -0.5 * tileSize


drawOrd :: Point3d -> Point3d -> Ordering
drawOrd (x1,y1,z1) (x2,y2,z2) = compare (z1,y1,x1) (z2,y2,x2)

drawThing :: Renderer -> Point3d -> (CInt,Texture) -> IO ()
drawThing r l (h,img) =
  do -- rendererDrawColor r $= V4 0 0 0 255
     copy r img Nothing (Just re)
     -- drawRect r (Just re)
  where
  (a,b) = coord l
  re    = Rectangle (P (V2 (a + cx) (b + cy))) (V2 tileSize h)
  cx    = - round (tileSize / 2)
  cy    = negate (h `div` 2)

type Thing = (CInt,Texture)
type Object = [Thing] -- infinite


appLoop :: [Object] ->  Renderer -> IO ()
appLoop objs r =
  do rendererDrawColor r $= V4 0 0 0 255
     clear r

     let thing i = head (objs !! i)
     let things = [ ((5,5,0), thing 0)
                  , ((6,5,0), thing 0)
                  , ((5,6,0), thing 0)
                  , ((6,6,0), thing 0)
                  , ((5,5,0.1), thing 1)
                  , ((5,5,3), thing 2)
                  , ((5,2,0), thing 4)
                  ] ++ trees
         trees = [ ((fromIntegral i, fromIntegral j, 0),
                          (objs !! 3) !! (i * j))
                  | i <- [ 7 .. 10 ], j <- [ 0 .. 5 ] ]
     let cmp (x,_) (y,_) = drawOrd x y
     mapM_ (uncurry (drawThing r)) (sortBy cmp things)

     present r

     events <- pollEvents
     unless (any isQpress events) $
       do threadDelay 16600
          appLoop (map tail objs) r

  where
  isQpress ev =
    case eventPayload ev of
      KeyboardEvent k ->
        keyboardEventKeyMotion k == Pressed &&
        keysymKeycode (keyboardEventKeysym k) == KeycodeQ
      _ -> False





