{-# Language TemplateHaskell, Rank2Types, OverloadedStrings #-}
module Board
  ( Board
  , boardNew
  , boardTiles
  , boardPawns
  , boardHasLoc

  , Pawns
  , newPawn
  , rmPawn
  , addPawn

  , testLayout

  , TileLoc, PawnLoc(..)
  , pawnTiles
  , pawnTilePower

  , InfluenceMap
  , addInfluence
  , influenceMap
  )
  where


import Data.Map(Map)
import qualified Data.Map as Map
import Control.Lens(makeLenses, (^.), (^?), ix)
import qualified Data.Text as Text
import Data.Ratio(Ratio, (%))

import Util.Perhaps

import Basics

-- | A region, containing an action to perform.
type TileLoc = (Int,Int)

-- | The location of a pawn, it may be a region or a road location.
data PawnLoc = PawnLoc Int Int
  deriving (Eq,Ord,Show)

-- | The current arrangement of the baord.
data Board = Board
  { _boardTiles   :: Map TileLoc Tile
    -- ^ The tiles on the board.

  , _boardPawns  :: Pawns
    -- ^ Multiple pawns may share a location by spending sharing tokens.
    -- The front is the top of the stack.
  }

-- | The pawns on the map.  Each location may have multiple pawn stacks,
-- if the pawns ended up sharing a space.
type Pawns = Map PawnLoc [PawnStack]
-- XXX support for pawn upgrades

$(makeLenses ''Board)






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

-- | Is the given location on the baord.
boardHasLoc :: Board -> PawnLoc -> Bool
boardHasLoc b l = any present (pawnTiles l)
  where present x = Map.member x (b ^. boardTiles)

-- | Remove a pawn for a player from a location.
-- If theere are multiple pawns, then remove the top one (i.e.,
-- the least powerful one).
-- Fails if there is no such pawn at the location.
rmPawn :: PawnLoc -> PlayerId -> Pawns -> Maybe (Pawn,Pawns)
rmPawn l pid mp =
  do ps <- Map.lookup l mp
     (q,qs) <- rm ps
     return (q,Map.insert l qs mp)
  where
  rm xs = case xs of
            []     -> Nothing
            a : as
              | a ^. pawnPlayer == pid -> Just (a,as)
              | otherwise -> do (b,bs) <- rm as
                                return (b,a:bs)

-- | Add a pawn to a location.  The new pawn goes on top (aka front).
addPawn :: PawnLoc -> Pawn -> Pawns -> Pawns
addPawn l p = Map.insertWith (++) l [p]

-- | Add a new pawn for the given player at the given location.
newPawn :: PawnLoc -> PlayerId -> Pawns -> Pawns
newPawn l p = addPawn l (pawnNew p)




--------------------------------------------------------------------------------

-- | Tiles where a pawn might have influence.  Note that this produces
-- the location of all tiles around the pawn,
-- even ones that are not not the board.
pawnTiles :: PawnLoc -> [TileLoc]
pawnTiles (PawnLoc x y) = (x',y') : more
  where
  (x',xr) = x `divMod` 2
  (y',yr) = y `divMod` 2

  left      = (x' - 1, y')
  below     = (x', y' - 1)
  leftBelow = (x' - 1, y' - 1)

  more =
    case (xr == 0, yr == 0) of
      (True,True)   -> [ below, leftBelow, left ]
      (True,False)  -> [ left ]
      (False,True)  -> [ below ]
      (False,False) -> []

-- | Influence exeretd by a pawn on the board.
pawnTilePower :: PawnLoc -> Pawn -> (Influence, [TileLoc])
pawnTilePower loc p = (pawnCurPower p % length locs, locs)
  where locs = pawnTiles loc


-- | For each tile, how much influence does each player have.
type InfluenceMap = Map TileLoc (Map PlayerId Influence)

type Influence = Ratio Int

-- | Add some influence to a particular tile on the map.
addInfluence :: PlayerId -> Influence -> TileLoc -> InfluenceMap -> InfluenceMap
addInfluence p x
  | x <= 0    = const id
  | otherwise = Map.alter $ \mb ->
    Just $
    case mb of
      Nothing -> Map.singleton p x
      Just mp -> Map.insertWith (+) p x mp

-- | Compute who controls each location on the map.
influenceMap :: Pawns -> InfluenceMap
influenceMap = Map.foldrWithKey upd Map.empty
  where
  upd l ps mp = foldr onePawnStack mp ps
    where
    onePawnStack

    onePawn p mp1 = foldr (addInfluence (p^.pawnPlayer) x) mp1 tps
      where (x,tps) = pawnTilePower l p






