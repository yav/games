{-# Language OverloadedStrings #-}
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

import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Ratio(Ratio, (%))
import           Data.Maybe(fromMaybe)
import           Control.Monad(unless,guard,msum)

import Util.Perhaps

import Pawn


data Tile = Tile

--------------------------------------------------------------------------------
-- Content Operations
--------------------------------------------------------------------------------

-- | Content of a location.
data Content = Content
  { contentStack   :: ![Pawn]  -- ^ The top is the first one
  , contentSharing :: ![Pawn]
  }

-- | No content.
contentNew :: Content
contentNew = Content { contentStack = [], contentSharing = [] }

-- | Is this a location with no pawns on it?
contentIsEmpty :: Content -> Bool
contentIsEmpty c = null (contentStack c) && null (contentSharing c)

-- | Add a pawn to this content.  If the pawn is "incognito" it
-- wil lgo on the side, otherwise it is going to go on the top of the stack.
contentAdd :: Pawn -> Content -> Content
contentAdd r c
  | pawnIncognito r = c { contentSharing = r : contentSharing c }
  | otherwise       = case contentStack c of
                            []     -> c { contentStack = [r] }
                            s : ss -> c { contentStack = r : pawnClear s : ss }

-- | Remove the given pawn from this location.
-- 'Nothing' if the pawn was not here, or is not on top of the stack.
contentRm :: PawnId -> Content -> Maybe (Pawn, Content)
contentRm pid c =
  case contentStack c of
    top : rest
      | thisPawn top -> Just (top, c { contentStack = rest })
    _ -> case break thisPawn (contentSharing c) of
           (as,b:bs) -> Just (b, c { contentSharing = as ++ bs })
           _         -> Nothing
  where
  thisPawn p = pawnId p == pid

contentActivePawns :: Content -> [Pawn]
contentActivePawns c = top ++ contentSharing c
  where top = case contentStack c of
                t : _ -> [t]
                []    -> []


--------------------------------------------------------------------------------
-- Locations

-- | A location of a tile on a board
type TileLoc = (Int,Int)

-- | A pawn location on the board, either a tile, edge, or a vertex.
data PawnLoc = PawnLoc Int Int
  deriving (Eq,Ord)


-- | Tiles where a pawn might have influence.  Note that this produces
-- the location of all tiles around the pawn,
-- even ones that are not not the board.
pawnLocTiles :: PawnLoc -> [TileLoc]
pawnLocTiles (PawnLoc x y) = (x',y') : more
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

-- | What tile does this pawn location correspond to, if any.
pawnLocOnTile :: PawnLoc -> Maybe TileLoc
pawnLocOnTile (PawnLoc x y) =
  do let (x',xr) = x `divMod` 2
         (y',yr) = y `divMod` 2
     guard (xr /= 0 && yr /= 0)
     return (x',y')

-- | Are these location adjacent.
pawnLocAdjacent :: PawnLoc -> PawnLoc -> Bool
pawnLocAdjacent (PawnLoc x1 y1) (PawnLoc x2 y2) = adj x1 x2 || adj y1 y2
  where
  adj a b = abs (a - b) == 1

-- | Is this location on the board?
pawnLocOnBoard :: Board -> PawnLoc -> Bool
pawnLocOnBoard b l = any (tileLocOnBoard b) (pawnLocTiles l)

-- | Is the given location part of this board.
tileLocOnBoard :: Board -> TileLoc -> Bool
tileLocOnBoard b l = Map.member l (boardTiles b)


--------------------------------------------------------------------------------
-- Computing influence

-- | Influence of a pawn is the pawn's strength spread evenly across
-- potential adjacent locations.
type Influence = Ratio Int

-- | Influence exeretd by a pawn on the board.
-- Note that some of the locations may be outside the board.
pawnTilePower :: PawnLoc -> Pawn -> (Influence, [TileLoc])
pawnTilePower loc p = (pawnCurPower p % length locs, locs)
  where locs = pawnLocTiles loc

-- | For each tile, how much influence does each player have.
type InfluenceMap = Map TileLoc (Map PlayerId Influence)

-- | Add some influence to a particular tile on the map.
addInfluence :: PlayerId -> Influence -> TileLoc -> InfluenceMap -> InfluenceMap
addInfluence p x =
  Map.alter (Just . Map.insertWith (+) p x . fromMaybe Map.empty)


--------------------------------------------------------------------------------
-- The Board

-- | The current arrangement of the baord.
data Board = Board
  { boardTiles   :: !(Map TileLoc Tile)
    -- ^ The tiles on the board.

  , boardContent :: !(Map PawnLoc Content)
    -- ^ Content for all valid board location.
  }


-- | A baord with the given layout and no pawns.
boardNew :: Map TileLoc Tile -> Board
boardNew ts =
  Board
    { boardTiles   = ts
    , boardContent = Map.empty
    }

-- | Remove a pawn from the board.
boarddRm :: PawnId -> Board -> Maybe (Pawn, Board)
boarddRm pid b =
  do (p,l,c) <- msum [ do (p,c1) <- contentRm pid c
                          return (p, l, c1)
                     | (l,c) <- Map.toList (boardContent b) ]
     return (p, b { boardContent = Map.insert l c (boardContent b) })


-- | Can this pawn enter the given location.
-- It assumes that the pawn is not currently on the board.
mayMove :: Pawn -> PawnLoc -> PawnLoc -> Board -> Bool
mayMove p lFrom lTo b
  | not (pawnLocOnBoard b lTo)      = False
  | not (pawnLocAdjacent lFrom lTo) = False
  | pawnIncognito p                 = True
  | otherwise =
    case pawnLocOnTile lTo of
      Nothing -> contentIsEmpty cnt
      Just tl  ->
        let othersI = Map.elems $ Map.delete owner $ influenceMap b1 Map.! tl
            ourI    = influenceMap b2 Map.! tl Map.! owner
        in all (ourI >) othersI

  where
  owner  = pidOwner (pawnId p)
  mbTile = pawnLocOnTile lTo
  cnt    = Map.findWithDefault contentNew lTo (boardContent b)

  b1     = uncheckedPlacePawn lFrom p b
  b2     = uncheckedPlacePawn lTo   p b


-- | Just add a pawn to a location.
-- If it is incognito, it will go to the side.
-- Otherwise it will go to the top of the stack.
uncheckedPlacePawn :: PawnLoc -> Pawn -> Board -> Board
uncheckedPlacePawn l p b =
  b { boardContent = Map.alter (Just . contentAdd p . fromMaybe contentNew)
                               l
                               (boardContent b) }


-- | Compute the players' influence for each location on the map.
influenceMap :: Board -> InfluenceMap
influenceMap b = Map.foldrWithKey upd Map.empty (boardContent b)
  where
  upd l c mp = foldr addPawn mp (contentActivePawns c)
    where
    addPawn p m = foldr (addInfluence owner amt) m existLoc
      where
      owner      = pidOwner (pawnId p)
      (amt,locs) = pawnTilePower l p
      existLoc   = filter (tileLocOnBoard b) locs






