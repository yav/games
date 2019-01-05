{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Game where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(when)


import Util.Perhaps

import Updater
import Ids
import Config
import Tile
import Area
import TurnOrder
import Player




--------------------------------------------------------------------------------
-- Promoting things

-- | Choose a market
data ChooseMarket = ChooseMarket
  { inArea      :: AreaId             -- ^ Going to this area
  , updatedArea :: Map MarketId Area  -- ^ One of these markets
  }

-- | Identifies any market in the game.
data GlobMarketId = GlobMarketId { gmAID :: AreaId, gmMID :: MarketId }
                      deriving (Eq,Ord)

-- | These need to be promoted.
data PromoteTodo = PromoteTodo
  { promExclude   :: [GlobMarketId] -- ^ Don't go there, as others already went
  , promStartFrom :: AreaId         -- ^ First area to try
  , promTile      :: OwnedTile      -- ^ This is the tile thwith needs promotion
  }

-- | Which of the excluded markets are on the current area
promExcludeStart :: PromoteTodo -> [ MarketId ]
promExcludeStart p = [ gmMID gmid | gmid <- promExclude p
                                  , gmAID gmid /= promStartFrom p ]




--------------------------------------------------------------------------------
-- Game

data Game         = Game { gameAreas    :: Map AreaId Area
                         , gamePlayers  :: Map PlayerId Player
                         , gamePalace   :: [OwnedTile]
                         , gameOrder    :: PlayerOrder
                         , gameRemoved  :: [OwnedTile]
                         , gameStatus   :: GameStatus
                         }



data GameStatus   = NextTurn
                    -- ^ At start of turn:
                    -- 1. click on vis (if any)
                    -- 2. click on stack (if any)
                    -- 3. click on empty spot of complete market (if any)
                    -- 4. click on vagrant if:
                    --     - (reinstate) empty space above, or
                    --     - (jump q) tile in last place in a full market
                    --     - (swap) tile in area, with a full market same spot occupied
                    --     - (move) tile in area, other area with 2, and empty

                  | ChooseNewTileLoc ChooseMarket   -- ^ (after 1 or 2)
                  | Promote ChooseMarket [PromoteTodo]  -- ^ (after 3)


                  | CompleteMarket AreaId MarketId MarketId
                    -- ^ After swap, and both markets are full

                  | GameFinished    -- ^ After end game



newGame :: [Player] -> Perhaps Game
newGame ps =
  do gameOrder <- newPlayerOrder (Map.keys gamePlayers)
     pure Game { .. }

  where
  pnum = length ps
  gamePlayers = Map.fromList (zip (playerIds pnum) ps)
  gameAreas = Map.fromList [ (i, emptyArea pnum) | i <- areaIds ]
  gamePalace = []
  gameRemoved = []
  gameStatus = NextTurn

--------------------------------------------------------------------------------
-- Complex fields of Game

area :: AreaId -> FieldOf Game Area
area aid = Field { .. }
  where
  getField g   = case Map.lookup aid (gameAreas g) of
                   Just a  -> a
                   Nothing -> emptyArea (numberOfPlayers (gameOrder g))
  setField a g = g { gameAreas = Map.insert aid a (gameAreas g) }

player :: PlayerId -> FieldOf Game Player
player pid = Field { .. }
  where
  getField g = case Map.lookup pid (gamePlayers g) of
                 Just p  -> p
                 Nothing -> error "Invalid player id"
  setField p g = g { gamePlayers = Map.insert pid p (gamePlayers g) }

turnOrder :: FieldOf Game PlayerOrder
turnOrder = Field { .. }
  where
  getField = gameOrder
  setField t g = g { gameOrder = t }
--------------------------------------------------------------------------------


gameCurPlayerId :: Game -> PlayerId
gameCurPlayerId = curPlayer . gameOrder


owned :: Tile -> Game -> OwnedTile
owned t g = OwnedTile { tileType = t, tileOwner = gameCurPlayerId g }


--------------------------------------------------------------------------------
addPalace :: OwnedTile -> Updater Game ()
addPalace ot = upd $ \g -> g { gamePalace = ot : gamePalace g }

whenReady :: Updater Game ()
whenReady =
  do status <- ask gameStatus
     case status of
       NextTurn     -> pure ()
       GameFinished -> failure "This game has finished."
       Promote {}   -> failure "Some tiles still need to be promoted."
       CompleteMarket {} ->
        failure "You need to choose which market to promote."

autoPromote :: [PromoteTodo] -> Updater Game ()
autoPromote ots =
  case ots of
    [] -> nextTurn
    todo : rest ->
      do let aid = promStartFrom todo
             ot  = promTile todo

         opts <- with (area aid)
                 $ ask $ autoPromoteArea (promExcludeStart todo) ot

         case opts of
           [] ->
             case nextArea aid of
               Nothing -> do addPalace ot
                             autoPromote rest
               Just aid1 -> autoPromote (todo { promStartFrom = aid1 }:rest)

           [(mid,a1)] ->
             do with (area aid) $ set a1
                let gmid = GlobMarketId { gmAID = aid, gmMID = mid }
                    avoid t = t { promExclude = gmid : promExclude t }
                autoPromote (map avoid rest)

           _ -> do let tgt = ChooseMarket
                               { inArea = aid
                               , updatedArea = Map.fromList opts }
                   upd $ \g -> g { gameStatus = Promote tgt rest }


nextTurn :: Updater Game ()
nextTurn =
  do atStart <- with turnOrder $ do advanceTurnOrder
                                    start <- ask firstPlayer
                                    cur   <- ask curPlayer
                                    pure (start == cur)
     when atStart checkGameFinished




addTile :: Updater Player Tile -> MarketId -> Updater Game ()
addTile chooseTile mid =
  do whenReady
     cur <- with turnOrder (ask curPlayer)
     t   <- with (player cur) chooseTile
     let ot = OwnedTile { tileType = t, tileOwner = cur }
     with (area firstArea) (addTileArea mid ot)
     nextTurn


checkGameFinished :: Updater Game ()
checkGameFinished =
  do promNum <- ask (length . gamePalace)
     pnum    <- with turnOrder (ask numberOfPlayers)
     when (promNum >= promotedEnds pnum) $
       upd $ \g -> g { gameStatus = GameFinished }


