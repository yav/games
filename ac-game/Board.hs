{-# Language TemplateHaskell, Rank2Types, OverloadedStrings #-}
module Board
  ( Board
  , boardNew
  , boardTiles
  , boardPawns
  , boardLocOwner

  , Pawns
  , newPawn
  , rmPawn
  , addPawn

  , testLayout

  , TileLoc, PawnLoc(..)
  , pawnTiles
  , pawnTilePower
  , pawnDistance

  , InfluenceMap
  , addInfluence
  , influenceMap
  )
  where


import Data.Map(Map)
import qualified Data.Map as Map
import Control.Lens(makeLenses, (^.), (^?), ix)

import Util.Perhaps

import Basics

type TileLoc = (Int,Int)

data PawnLoc = PawnLoc Int Int
  deriving (Eq,Ord)



data Board = Board
  { _boardTiles   :: Map TileLoc Tile
    -- ^ The tiles on the board.

  , _boardPawns  :: Pawns
    -- ^ Multiple pawns may share a location by spending sharing tokens.
    -- The front is the top of the stack.
  }

type Pawns = Map PawnLoc [Pawn]

$(makeLenses 'Board)

testLayout :: Map TileLoc Tile
testLayout = Map.fromList
  [ ((x,y),tile)
  | (y,row) <- ixed [ [ UpgradePower, UpgradeSpeed, GainPawn ]
                    , map GainToken [ PowerBoost, SpeedBoost, ShareSpace ]
                    , [ InvsetInPower, InverstInSpeed, InvestInSharing ]
                    ]
  , (x,tile) <- ixed row
  ]
  where
  ixed = zip [ 0 .. ]

-- | Make up a new board.
boardNew :: Map TileLoc Tile -> Board
boardNew ts = Board { _boardTiles = ts, _boardPawns = Map.empty }

-- | Get the player who owns a particular location, if any.
boardLocOwner :: PawnLoc -> Board -> Perhaps (Maybe PlayerId)
boardLocOwner l b =
  do ps <- perhaps "Location not on the board." (b ^? boardPawns . ix l)
     return (ps ^? ix 0 . pawnPlayer)


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

-- | Streight-line distance between two locations.
-- Note that this does not account for the layout of the board
-- (e.g., gaps, obstacles, etc.)
pawnDistance :: PawnLoc -> PawnLoc -> Int
pawnDistance (PawnLoc x y) (PawnLoc a b) = max (abs (x - a)) (abs (y - b))

-- | Who controls the tiles on the map,
-- and by who much.
type InfluenceMap = Map TileLoc (PlayerId,Int)

-- | Add some influence to a particular tile on the map.
addInfluence :: PlayerId -> Int -> TileLoc -> InfluenceMap -> InfluenceMap
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
influenceMap :: Pawns -> InfluenceMap
influenceMap = Map.foldrWithKey upd Map.empty
  where
  upd l ps mp = foldr onePawn mp ps
    where
    onePawn p mp1 = foldr (addInfluence (p^.pawnPlayer) x) mp1 tps
      where (x,tps) = pawnTilePower l p






