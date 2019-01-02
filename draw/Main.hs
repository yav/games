{-# Language OverloadedStrings #-}
import Data.Word
import Data.Maybe(fromMaybe)
import Control.Monad(unless,msum,mplus)
import SDL
import SDL.Primitive
import qualified SDL.Framerate as FR
import qualified Data.Vector.Storable as Vector

import qualified Util.Hex as Hex

main :: IO ()
main =
  do initializeAll
     w <- createWindow "Yav 1" defaultWindow
     r <- createRenderer w (-1) defaultRenderer
     FR.with 400 $ \fm -> go r fm mStart
  where
  go r fm m =
    do t <- FR.delay fm
       es <- pollEvents
       let m1 = mUpdate t es m
       mDraw r m1
       present r
       unless (done m1) (go r fm m1)

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



mDraw :: Renderer -> M -> IO ()
mDraw r m =
  do rendererDrawColor r $= V4 0 0 0 255
     clear r
     let c = V4 0 0 255 255
     mapM_ (drawHex r c) [ Hex.Loc x y | x <- [ 1 .. 10 ], y <- [ 1 .. 10 ] ]
     drawHex r (V4 255 0 0 255) (hLoc m)
     -- let rect = Rectangle (fromIntegral <$> loc m) (V2 50 50)
     -- fillRect r (Just rect)
     let P (V2 x' y') = loc m
         x = fromIntegral x'
         y = fromIntegral y'
     -- fillRoundRectangle r (V2 x y) (V2 (x+50) (y+50)) 5 (V4 255 0 255 255)
     smoothCircle r (V2 x y) 50 (V4 255 0 255 255)

-- drawHex :: Renderer -> VHex.Loc -> IO ()
drawHex r col l = fillPolygon r (Vector.fromList xs) (Vector.fromList ys) col
  where
  (xs,ys) = unzip [ (round x, round y) | (x,y) <- Hex.locPoints 32 24 l ]


