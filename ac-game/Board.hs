{-# Language TemplateHaskell, Rank2Types #-}
module Board
  ( Board
  , boardNew
  , boardTiles
  , boardPawns

  , newPawn
  , rmPawn
  , addPawn
  , movePawn

  , TileLoc, PawnLoc
  , pawnTiles
  , pawnTilePower

  , InfuenceMap
  , addInfluence
  , influenceMap
  )
  where


import Data.Map(Map)
import qualified Data.Map as Map
import Control.Lens(makeLenses, (^.))

import Basics

type TileLoc = (Int,Int)

data PawnLoc = PawnLoc Int Int
  deriving (Eq,Ord)



data Board = Board
  { _boardTiles'  :: Map TileLoc Tile
    -- ^ The tiles on the board.
    -- These are probably not going to change.


  , _boardPawns  :: Pawns
    -- ^ Multiple pawns may share a location by spending sharing tokens.
    -- The front is the top of the stack.
  }

type Pawns = Map PawnLoc [Pawn]

$(makeLenses 'Board)


-- | Make up a new board.
boardNew :: Map TileLoc Tile -> Board
boardNew ts = Board { _boardTiles' = ts, _boardPawns = Map.empty }

boardTiles :: Board -> Map TileLoc Tile
boardTiles b = b ^. boardTiles'



-- | Remove a specific pawn from a location.
-- If theere are multiple pawns that are the same, then
-- remove the bottom one (i.e., the last one).
-- Fails if there is no such pawn at the location.
rmPawn :: PawnLoc -> Pawn -> Pawns -> Maybe Pawns
rmPawn l p mp =
  do ps <- Map.lookup l mp
     qs <- rm ps
     return (Map.insert l qs mp)

  where
  rm xs = case xs of
            []     -> Nothing
            a : as -> case rm as of
                        Nothing | a == p    -> Just as
                                | otherwise -> Nothing
                        Just bs -> Just (a : bs)

-- | Add a pawn to a location.  The new pawn goes on top (aka front).
addPawn :: PawnLoc -> Pawn -> Pawns -> Pawns
addPawn l p = Map.insertWith (++) l [p]

-- | Add a new pawn for the given player at the given location.
newPawn :: PawnLoc -> PlayerId -> Pawns -> Pawns
newPawn l p = addPawn l (pawnNew p)


-- | Move a pawn from one location to another, without considering any rules.
movePawn :: PawnLoc -> PawnLoc -> Pawn -> Pawns -> Maybe Pawns
movePawn from to p b = addPawn to p <$> rmPawn from p b



--------------------------------------------------------------------------------

{- | Here's a diagram explaining the geometry:

@
  *-*-*
  |\|/|
1 b-d-*
  |/|\|
0 a-c-*
  0 1
@

'PawnLoc' @(x,y)@ refers to:

  * Both even:          vertex
  * @x@ even, @y@ odd:  vertical edge
  * @x@ odd,  @y@ even: horizontal edge
  * Both odd:           face
-}
pawnTiles :: PawnLoc -> [TileLoc]
pawnTiles (PawnLoc x y) = (x',y') : more
  where
  (x',xr) = x `divMod` 2
  (y',yr) = y `divMod` 2

  left      = (x' - 1, y')
  below     = (x', y' - 1)
  leftBelow = (x' - 1, y - 1)

  more =
    case (xr == 0, yr == 0) of
      (True,True)   -> [ below, leftBelow, left ]
      (True,False)  -> [ left ]
      (False,True)  -> [ below ]
      (False,False) -> []

-- | Influence exeretd by a pawn on the board.
pawnTilePower :: PawnLoc -> Pawn -> (Int, [TileLoc])
pawnTilePower loc p = (p^.pawnPower `div` length locs, locs)
  where locs = pawnTiles loc



-- | Who controls the tiles on the map,
-- and by who much.
type InfuenceMap = Map TileLoc (PlayerId,Int)

-- | Add some influence to a particular tile on the map.
addInfluence :: PlayerId -> Int -> TileLoc -> InfuenceMap -> InfuenceMap
addInfluence p x
  | x <= 0    = const id
  | otherwise = Map.alter $ \mb ->
    case mb of
      Nothing -> Just (p,x)
      Just (p1,y)
        | p == p1 -> Just (p, x + y)
        | otherwise ->
          case compare x y of
            GT -> Just (p, x - y)
            EQ -> Nothing
            LT -> Just (p1, y - x)

-- | Compute who controls each location on the map.
influenceMap :: Pawns -> Map TileLoc (PlayerId,Int)
influenceMap = Map.foldWithKey upd Map.empty
  where
  upd l ps mp = foldr onePawn mp ps
    where
    onePawn p mp1 = foldr (addInfluence (p^.pawnPlayer) x) mp1 tps
      where (x,tps) = pawnTilePower l p






