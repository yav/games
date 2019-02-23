{-# Language OverloadedStrings #-}
import Data.Word
import Data.Maybe(fromMaybe)
import Control.Monad(unless,msum,mplus)
import SDL
import SDL.Primitive
import qualified SDL.Framerate as FR
import SDL.Font (Font)
import qualified SDL.Font as Font
import qualified Data.Vector.Storable as Vector

import qualified Util.Hex as Hex

import Bezier

main :: IO ()
main =
  do initializeAll
     Font.initialize
     w <- createWindow "Yav 1" defaultWindow
     r <- createRenderer w (-1) defaultRenderer
     font <- Font.load "data/Coiny-Regular.ttf" 64
     FR.with 400 $ \fm ->
        let res = Resources { rFRManager = fm
                            , rWindow = w
                            , rRenderer = r
                            , rFont = font
                            }
        in go res mStart
  where
  go res m =
    do t <- FR.delay (rFRManager res)
       es <- pollEvents
       let m1 = mUpdate t es m
       mDraw res m1
       present (rRenderer res)
       unless (done m1) (go res m1)

data Resources = Resources
  { rFont :: Font
  , rWindow :: Window
  , rRenderer :: Renderer
  , rFRManager :: FR.Manager
  }

data M = M { loc :: !(Point V2 Word), done :: !Bool, mTime :: !Int
           , hLoc :: !Hex.Loc }

mStart :: M
mStart = M { loc = P (V2 0 0), done = False, mTime = 0, hLoc = Hex.Loc 1 1 }

mUpdate :: Int -> [Event] -> M -> M
mUpdate t es m =
  M { done = any quit es
    , loc  = leftRight
    , hLoc = followMouse
    , mTime = t + mTime m
    }
  where

  leftRight = case loc m of
                P (V2 x y) -> P (V2 x' y')
                  where
                  upd d f = round (d + d * f (fromIntegral (mTime m) / 1000))
                  x' = upd 300 sin
                  y' = upd 300 cos

  followMouse = case foldr checMouse Nothing es of
                  Nothing -> hLoc m
                  Just (P (V2 x y)) ->
                    Hex.pixelToLoc 32 (fromIntegral x, fromIntegral y)

  checMouse ev have = have `mplus` mouse ev
  mouse e =
    case eventPayload e of
      MouseMotionEvent m -> Just (fromIntegral <$> mouseMotionEventPos m)
      _                  -> Nothing

  quit e =
    case eventPayload e of
      KeyboardEvent k
        | Pressed  <- keyboardEventKeyMotion k
        , KeycodeQ <- keysymKeycode (keyboardEventKeysym k) -> True
      _ -> False



mDraw :: Resources -> M -> IO ()
mDraw res m =
  do let r = rRenderer res
     rendererDrawColor r $= V4 0 0 0 255
     clear r
     let c = V4 0 0 255 255
     msg <- Font.shaded (rFont res) (V4 255 255 255 255)
                                    (V4 0 0 0 255)
                                     "Hello"
     msgTxt <- createTextureFromSurface r msg
     freeSurface msg
     -- copy r msgTxt Nothing Nothing


     mapM_ (drawHex r c) [ Hex.Loc x y | x <- [ 1 .. 10 ], y <- [ 1 .. 10 ] ]
     drawHex r (V4 255 0 0 255) (hLoc m)
     -- let rect = Rectangle (fromIntegral <$> loc m) (V2 50 50)
     -- fillRect r (Just rect)
     let P (V2 x' y') = loc m
         x = fromIntegral x'
         y = fromIntegral y'
     -- fillRoundRectangle r (V2 x y) (V2 (x+50) (y+50)) 5 (V4 255 0 255 255)
     smoothCircle r (V2 x y) 50 (V4 255 0 255 255)

     let p1 = From (To 0, To 100) $ Toward (By 50, By 50)
                       $ EndPt 1 (By 50,  By (-50))
              $ Smooth $ EndPt 1 (By 100, By 0)
              $ Smooth $ EndPt 1 (By 100, By 0)
              $ Sharp  $ Toward (By 20, By 50) $ EndPt 1 (By 20, By (-50))
              -- $ End
              $ Smooth $ EndVec 1 (To 280, To 100) (To 300, To 200)
              $ End
 
         p2 = From (To 300, To 200) $ Toward (By 20, By 100)
              $ EndPt 1 (By 100, By 0) $ End

     drawCurve r (V4 0 255 255 255) $ path p1

     drawCurve r (V4 255 0 0 255) $ path p2

     -- drawCurve r (V4 255 255 0 255) $ path (Join p1 1 p2)

-- drawHex :: Renderer -> VHex.Loc -> IO ()
drawHex r col l = fillPolygon r (Vector.fromList xs) (Vector.fromList ys) col
  where
  (xs,ys) = unzip [ (round x, round y) | (x,y) <- Hex.locPoints 32 24 l ]


dLine :: Renderer -> (Double,Double) -> (Double,Double) -> IO ()
dLine r from to = smoothLine r (pt from) (pt to) (V4 255 0 0 255)
  where
  pt (x,y) = V2 (round x) (round y)

drawCurve :: Renderer -> V4 Word8 -> (Double -> Pt) -> IO ()
drawCurve r c f = mapM_ pt (takeWhile (<= 1) (iterate (+ step) 0))
  where
  step = 0.005
  pt t = let Pt x y = f t
         in fillCircle r (V2 (round x) (round y)) 3 c

