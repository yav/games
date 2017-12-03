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

import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Ratio(Ratio, (%))
import           Data.Maybe(fromMaybe)
import           Control.Lens
import           Control.Monad(unless,guard)

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

  , _boardContent :: !(Map PawnLoc Content)
    -- ^ Content for all valid board location.
  }

data Content = Content
  { _contentStack   :: ![Pawn]  -- ^ The top is the first one
  , _contentSharing :: ![Pawn]
  }

data Tile = Tile


$(makeLenses ''Board)
$(makeLenses ''Content)

--------------------------------------------------------------------------------
-- Content Operations
--------------------------------------------------------------------------------

-- | No content.
contentNew :: Content
contentNew = Content { _contentStack = [], _contentSharing = [] }

-- | Is this a location with no pawns on it?
contentIsEmpty :: Content -> Bool
contentIsEmpty c = null (c ^. contentStack) && null (c ^. contentSharing)


-- | Add a pawn to this content.  If the pawn is "incognito" it
-- wil lgo on the side, otherwise it is going to go on the top of the stack.
contentAdd :: Pawn -> Content -> Content
contentAdd r c
  | r ^. pawnIncognito = c & contentSharing %~ (r :)
  | otherwise          = case c ^. contentStack of
                            []     -> c & contentStack .~ [r]
                            s : ss -> c & contentStack .~ r : pawnClear s : ss

-- | Remove the given pawn from this location.
-- 'Nothing' if the pawn was not here, or is not on top of the stack.
contentRm :: PawnId -> Content -> Maybe (Pawn, Content)
contentRm pid c =
  case c ^. contentStack of
    top : rest
      | thisPawn top -> Just (top, c & contentStack .~ rest)
    _ -> case break thisPawn (c ^. contentSharing) of
           (as,b:bs) -> Just (b, c & contentSharing .~ (as ++ bs))
           _         -> Nothing
  where
  thisPawn p = p ^. pawnId == pid

contentActivePawns :: Content -> [Pawn]
contentActivePawns c = top ++ (c ^. contentSharing)
  where top = case c ^. contentStack of
                t : _ -> [t]
                []    -> []


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

-- | What tile does this pawn location correspond to, if any.
pawnLocOnTile :: PawnLoc -> Maybe TileLoc
pawnLocOnTile (PawnLoc x y) =
  do let (x',xr) = x `divMod` 2
         (y',yr) = y `divMod` 2
     guard (xr /= 0 && yr /= 0)
     return (x',y')


type Influence = Ratio Int

-- | Influence exeretd by a pawn on the board.
-- The locations returned may not be on the board.
pawnTilePower :: PawnLoc -> Pawn -> (Influence, [TileLoc])
pawnTilePower loc p = (pawnCurPower p % length locs, locs)
  where locs = pawnTiles loc

-- | For each tile, how much influence does each player have.
type InfluenceMap = Map TileLoc (Map PlayerId Influence)

-- | Add some influence to a particular tile on the map.
addInfluence :: PlayerId -> Influence -> TileLoc -> InfluenceMap -> InfluenceMap
addInfluence p x
  | x <= 0    = const id
  | otherwise = Map.alter $ \mb ->
    Just $
    case mb of
      Nothing -> Map.singleton p x
      Just mp -> Map.insertWith (+) p x mp


--------------------------------------------------------------------------------
-- Board Operations

-- | A baord with the given layout and no pawns.
boardNew :: Map TileLoc Tile -> Board
boardNew ts =
  Board
    { _boardTiles   = ts
    , _boardContent = Map.empty
    }

-- | Can this pawn enter the given location.
mayEnter :: Pawn -> PawnLoc -> Board -> Bool
mayEnter p l b
  | not (onBoardLoc b l)  = False
  | p ^. pawnIncognito    = True
  | otherwise =
    case pawnLocOnTile l of
      Nothing -> contentIsEmpty cnt
      Just tl  ->
        let othersI = Map.elems $ Map.delete owner $ influenceMap b Map.! tl
            ourI    = influenceMap b1 Map.! tl Map.! owner
        in all (ourI >) othersI

  where
  owner  = pidOwner (p ^. pawnId)
  mbTile = pawnLocOnTile l
  cnt    = fromMaybe contentNew (b ^. boardContent . at l)
  b1     = uncheckedPlacePawn l p b



uncheckedPlacePawn :: PawnLoc -> Pawn -> Board -> Board
uncheckedPlacePawn l p b =
  b & boardContent . at l %~ (Just . contentAdd p . fromMaybe contentNew)



-- | Compute the players' influence for each location on the map.
influenceMap :: Board -> InfluenceMap
influenceMap b = Map.foldrWithKey upd Map.empty (b ^. boardContent)
  where
  upd l c mp = foldr addPawn mp (contentActivePawns c)
    where
    addPawn p m = foldr (addInfluence owner amt) m existLoc
      where
      owner      = pidOwner (p ^. pawnId)
      (amt,locs) = pawnTilePower l p
      existLoc   = filter (onBoardTile b) locs

-- | Is the given location part of this board.
onBoardTile :: Board -> TileLoc -> Bool
onBoardTile b l = Map.member l (b ^. boardTiles)

onBoardLoc :: Board -> PawnLoc -> Bool
onBoardLoc b l = any (onBoardTile b) (pawnTiles l)




