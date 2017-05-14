-- module Hex where

import Graphics.Gloss.Interface.Pure.Game
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(replicateM)

import Util.Random
import Util.ResourceQ
import Util.Hex

main :: IO ()
main =
  do rnd <- randSourceIO
     play (InWindow "Hex" (600,600) (100,0)) black 0
        (gameInit rnd) gameDraw gameInput gameUpdate

data Game = Game
  { gWorld  :: World
  , gScale  :: Float
  , gTrans  :: (Float,Float)
  , gTiles  :: Int
  }


gUpdateWorld :: (World -> World) -> Game -> Game
gUpdateWorld f g = g { gWorld = f (gWorld g) }

gMove :: (Float,Float) -> Game -> Game
gMove (dx,dy) g = g { gTrans = (x + dx, y + dy) }
  where (x,y) = gTrans g

gameInit :: StdGen -> Game
gameInit rnd =
  genRandFun rnd $
    do s : ss <- shuffle pieces
       let next = BoardPiece { pPiece = s, pLoc = Loc 0 0, pDir = NE }
       return $ \rnd ->
        Game { gWorld = World { wBoard = Map.empty
                              , wNextPiece = next
                              , wPieces = ss
                              }
             , gScale = 40
             , gTrans = (0,0)
             , gTiles = 0
             }

gameInput :: Event -> Game -> Game
gameInput ev w =
  case ev of
    EventKey (Char k) Down mods _ ->
      case k of
        'i' -> gUpdateWorld (wUpdateNext (bpMove NW)) w
        'o' -> gUpdateWorld (wUpdateNext (bpMove NE)) w
        'l' -> gUpdateWorld (wUpdateNext (bpMove E )) w
        ',' -> gUpdateWorld (wUpdateNext (bpMove SE)) w
        'm' -> gUpdateWorld (wUpdateNext (bpMove SW)) w
        'j' -> gUpdateWorld (wUpdateNext (bpMove W )) w

        'k' -> gUpdateWorld (wUpdateNext (bpRot 1)) w
        'K' -> gUpdateWorld (wUpdateNext (bpRot (-1))) w

        'f' -> gUpdateWorld (wUpdateNext bpFlip) w

        'Z'  -> w { gScale = gScale w + 10 }
        'z' -> w { gScale = max 1 (gScale w - 10) }

        _   -> w

    EventKey (SpecialKey spec) Up _ _ ->
      case spec of
        KeySpace -> let w1 = gUpdateWorld wPlaceNext w
                    in case wPieces (gWorld w) of
                         [] -> w1
                         _  -> w1 { gTiles = 1 + gTiles w }

        KeyLeft -> gMove (-10,0) w
        KeyRight -> gMove (10,0) w
        KeyUp-> gMove (0,10) w
        KeyDown -> gMove (0,-10) w



        _ -> w

    _ -> w


gameUpdate :: Float -> Game -> Game
gameUpdate t w = w

gameDraw :: Game -> Picture
gameDraw g = pictures
  [ translate (fst (gTrans g)) (snd (gTrans g))
            $ scale (gScale g) (gScale g)
            $ pictures [ drawBoard (wBoard w), drawBoardPiece c (wNextPiece w) ]
  , translate (-280) 250 $ scale 0.5 0.5 $ color white $ text $ show $ gTiles g
  ]
  where
  w = gWorld g
  c = if bpValid (wBoard w) (wNextPiece w) then white else red



--------------------------------------------------------------------------------


data World = World
  { wBoard      :: Map Loc Terrain
  , wNextPiece  :: BoardPiece
  , wPieces     :: [Piece2]
  }

wUpdateNext :: (BoardPiece -> BoardPiece) -> World -> World
wUpdateNext f w = w { wNextPiece = f (wNextPiece w) }

wPlaceNext :: World -> World
wPlaceNext w =
  case wPieces w of
    [] -> w
    p : ps ->
     let next = BoardPiece { pPiece = p
                           , pLoc = pLoc (wNextPiece w)
                           , pDir = NW }
     in World { wBoard = place (wNextPiece w) (wBoard w)
               , wNextPiece = next
               , wPieces = ps
               }


--------------------------------------------------------------------------------
type Board = Map Loc Terrain

place :: BoardPiece -> Board -> Board
place p b0 = snd (foldl add (d0, Map.insert l c b0) xs)
  where
  add (d,b) t = (turn 1 d, Map.insert (move 1 d l) t b)
  Piece c xs = piece2Cur (pPiece p)
  d0 = pDir p
  l = pLoc p

boardAround :: Board -> [Loc]
boardAround b =
  [ l1 | l <- Map.keys b, l1 <- neighbours l, not (Map.member l1 b) ]

boardDiam :: Board -> Int
boardDiam b = case boardAround b of
               [] -> 0
               [_] -> 0
               x : xs -> maximum (map (d x) xs)
  where
  d (Loc x1 y1) (Loc x2 y2) = max (abs (x1 - x2)) (abs (y1 - y2))

--------------------------------------------------------------------------------
data BoardPiece = BoardPiece
  { pPiece :: Piece2
  , pDir   :: Dir
  , pLoc   :: Loc
  } deriving Show


bpRot :: Int -> BoardPiece -> BoardPiece
bpRot n p = p { pDir = turn n (pDir p) }

bpMove :: Dir -> BoardPiece -> BoardPiece
bpMove d p = p { pLoc = move 1 d (pLoc p) }

bpFlip :: BoardPiece -> BoardPiece
bpFlip p = p { pPiece = piece2Flip (pPiece p) }

bpSats :: BoardPiece -> Int
bpSats p = case piece2Cur (pPiece p) of
             Piece _ xs -> length xs

bpLocs :: BoardPiece -> [Loc]
bpLocs p = pLoc p : map sat (take (bpSats p) (dropWhile (/= pDir p) dirCycle))
  where
  sat d = move 1 d (pLoc p)

bpAround :: BoardPiece -> [Loc]
bpAround = Set.toList . Set.fromList . concatMap neighbours . bpLocs

bpValid :: Board -> BoardPiece -> Bool
bpValid b p = not overlaps && connected
  where
  overlaps = any (`Map.member` b) locs
  connected = check [ xs | l <- bpLocs p, let xs = conLoc l, not (null xs) ]

  locs = bpLocs p
  conLoc l = [ l1 | l1 <- neighbours l, l1 `Map.member` b ]

  check (xs : xss) = any (\x -> any (/= [x]) xss) xs || check xss
  check [] = False

--------------------------------------------------------------------------------
dirCycle :: [Dir]
dirCycle = cycle allDirs

--------------------------------------------------------------------------------

data Piece2 = Piece2 Piece Piece deriving (Eq,Show)

piece2Cur :: Piece2 -> Piece
piece2Cur (Piece2 x _) = x

piece2Flip :: Piece2 -> Piece2
piece2Flip (Piece2 x y) = Piece2 y x


data Piece = Piece Terrain [Terrain] deriving (Eq,Show)


pieceFlip :: Piece -> Piece
pieceFlip (Piece x xs) = Piece x (reverse xs)

tri :: Terrain -> Terrain -> Terrain -> Piece
tri x y z = Piece x [y,z]

rndTri :: Gen Piece
rndTri =
  do x <- rndTerrain
     xs <- replicateM 2 rndTerrain
     return (Piece x xs)

data Terrain = Field | Forest | Crops | Water | Mountain
                deriving (Eq,Ord,Show,Enum,Bounded)

rndTerrain :: Gen Terrain
rndTerrain = oneOf [ minBound .. maxBound ]


--------------------------------------------------------------------------------

drawBoard :: Board -> Picture
drawBoard = pictures . map draw . Map.toList
  where draw (l,t) = loc l (drawTerrain t)


ghex :: Dir -> Int -> Picture
ghex d s = polygon (start ++ take (s+1) [ (sin a, cos a)
                                    | n <- [ n0 .. ]
                                    , let a = fromIntegral n * step ])
  where
  step = pi / 3
  start = if s <= 5 then [(0,0)] else []
  n0 = fromEnum d

hex :: Picture
hex = bhex NE 0 white

bhex :: Dir -> Int -> Color -> Picture
bhex d n c = pictures [ color c (ghex d n)
                      , ghex (turn n d) (6 - n)
                      , scale 0.9 0.9 (ghex NE 6) ]


drawTerrain :: Terrain -> Picture
drawTerrain t = color (terrainColor t) hex


terrainColor :: Terrain -> Color
terrainColor t =
  case t of
    Field -> light green
    Forest -> dark (dark green)
    Crops  -> yellow
    Water  -> blue
    Mountain -> greyN 0.5



drawBoardPiece :: Color -> BoardPiece -> Picture
drawBoardPiece c p =
  pictures (center : zipWith satellite (iterate (turn 1) d0) xs)
  where
  Piece x xs  = piece2Cur (pPiece p)
  l           = pLoc p
  d0          = pDir p
  n           = length xs
  satellite d s = loc (move 1 d l)
                $ color (terrainColor s)
                $ bhex (turn (-1) d) 3 c
  center = loc l
         $ color (terrainColor x)
         $ bhex (turn n d0) (6 - n) c

loc :: Loc -> Picture -> Picture
loc (Loc x' y') = translate (2 * x * rr + y * rr) (y * 1.5)
  where
  x   = fromIntegral x'
  y   = fromIntegral y'
  rr  = cos (pi/6)

--------------------------------------------------------------------------------

{-
placeOpts :: Board -> Piece -> [BoardPiece]
placeOpts b p = [ x | l <- boardAround b, x <- opts l ]
  where
  mk l d = BoardPiece { pPiece = p, pDir = d, pLoc = l }
  spin l = [ x | d <- allDirs, let x = mk l d, bpValid b x ]
  opts l = [ x | d <- allDirs, x <- spin (move 1 d l) ] ++ spin l


gen :: [Piece] -> [Board]
gen [] = [ Map.empty ]
gen [p] =
  [ place BoardPiece { pPiece = p, pLoc = Loc 0 0, pDir = NE } Map.empty ]
gen (p : ps) = [ b | a <- gen ps
                   , b <- case placeOpts a p of
                            [] -> [a]
                            qs -> [ place q a | q <- qs, let Loc x y = pLoc q
                                              , abs x < 10, abs y < 10 ]
                   ]


-}

--------------------------------------------------------------------------------

pieces :: [Piece2]
pieces = let oneSide = ps (Mountain:normal)
             oneSide' = ps (Water:normal)
         in zipWith Piece2 oneSide (map (other Water) oneSide) ++
            zipWith Piece2 oneSide' (map (other Mountain) oneSide')

  {-[ p | s <- special, p <- ps (s : normal) ]
        ++ concat (replicate 5
                      ([ p | s <- special, p <- Piece s [s,s]
                                    : [ Piece s [s,n] | n <- normal ] ]))-}
  where
  other w (Piece x [y,z])
    | x == y && x == z = Piece w [w,w]
    | otherwise = Piece x [w,w]

  border = [ Piece n [Water,Water] | n <- normal ]

  special = [ Water, Mountain ]
  normal  = [ Field, Forest, Crops ]

  ps xs  = ps1 xs ++ ps2 xs ++ ps3 xs

  ps1 xs = [ Piece x [x,x] | x <- xs ]
  ps2 xs = [ Piece x [x,y] | (x,ys) <- pickOne xs, y <- ys ]
  ps3 xs = [ Piece x [y,z] | (x,ys) <- pickOne xs, (y,zs) <- pickOne ys, z <- zs ]

pickOne :: [a] -> [(a,[a])]
pickOne (x : xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- pickOne xs ]
pickOne []       = []


pieces6 :: [Piece]
pieces6 = [ p | s <- special, p <- ps (s : normal) ]
  where
  special = [ Water, Mountain ]
  normal  = [ Field, Forest, Crops ]

  ps xs  = ps1 xs ++ ps2 xs ++ ps3 xs

  ps1 xs = [ Piece x (replicate 6 x) | x <- xs ]
  ps2 xs = [ Piece x (replicate 3 x ++ replicate 3 y)
                                        | (x,ys) <- pickOne xs, y <- ys ]
  ps3 xs = [ Piece x (replicate 2 x ++ replicate 2 y ++ replicate 2 z)
                        | (x,ys) <- pickOne xs, (y,zs) <- pickOne ys, z <- zs ]



