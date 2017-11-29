{-# Language TemplateHaskell, Rank2Types, OverloadedStrings #-}
module Board where {-
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
-}

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Lens
import qualified Data.Text as Text
import Data.Ratio(Ratio, (%))

import Util.Perhaps

import Pawn


-- | A location of a tile on a board
type TileLoc = (Int,Int)

-- | A pawn location on the board, either a tile, edge, or a vertex.
data PawnLoc = PawnLoc Int Int
  deriving (Eq,Ord)


-- | The current arrangement of the baord.
data Board = Board
  { _boardTiles   :: !(Map TileLoc Tile)
    -- ^ The tiles on the board.

  , _boardPawns   :: !(Map PawnLoc Content)
    -- The front is the top of the stack.
  }

data Content = Content
  { _contentStack   :: ![Pawn]  -- ^ The top is the first one
  , _contentSharing :: ![Pawn]
  }

data Tile = Tile


$(makeLenses ''Board)
$(makeLenses ''Content)

--------------------------------------------------------------------------------
-- Basic Constructors
--------------------------------------------------------------------------------

-- | A baord with the given layout and no pawns.
boardNew :: Map TileLoc Tile -> Board
boardNew ts =
  Board
    { _boardTiles = ts
    , _boardPawns = Map.empty
    }

-- | No content.
contentNew :: -> Content
contentNew = Content { _contentStack = [], _contentSharing = [] }


--------------------------------------------------------------------------------


-- | Add a pawn to this content.
contentAdd :: Pawn -> Content -> Content
contentAdd r c
  | r ^. pawnIncognito = c & contentSharing %~ (r :)
  | otherwise          = case c ^. contentStack of
                            []     -> c & contentStack .~ [r]
                            s : ss -> c & contentStack .~ r : pawnClear s : ss

contentRm :: PawnId -> Content -> Maybe (Pawn, Content)
contentRm pid c =
  case c ^. contentStack . stackTop . pawnId of
    Just pid' | pid == pid' -> con
    _ ->




{-

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

-}




