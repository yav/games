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

-- | Where a promoted tile can go.
data PromotionTarget = PromotionTarget
  { promoteArea     :: AreaId             -- ^ Going to this area
  , promoteMarkets  :: Map MarketId Area  -- ^ One of these markets
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
                  | GameFinished
                  | Promote PromotionTarget [PromoteTodo]
                  | CompleteMarket AreaId MarketId MarketId



firstArea :: AreaId
firstArea = AreaId 0

nextArea :: AreaId -> Maybe AreaId
nextArea (AreaId x)
  | y < areaNum = Just (AreaId y)
  | otherwise   = Nothing
  where y = x + 1

newGame :: [Player] -> Perhaps Game
newGame ps =
  do gameOrder <- newPlayerOrder (Map.keys gamePlayers)
     pure Game { .. }

  where
  pnum = Map.size gamePlayers
  gamePlayers = Map.fromList [ (PlayerId i, p) | (i,p) <- zip [0..] ps ]
  gameAreas = Map.fromList [ (AreaId i, emptyArea pnum)
                              | i <- take areaNum [0..]]
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

           _ -> do let tgt = PromotionTarget
                               { promoteArea    = aid
                               , promoteMarkets = Map.fromList opts }
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

owned :: Tile -> Game -> OwnedTile
owned t g = OwnedTile { tileType = t, tileOwner = gameCurPlayerId g }

