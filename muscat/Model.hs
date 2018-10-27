{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
import Control.Monad(unless,when)

import qualified Data.Map as Map

import Updater
import Ids
import Tile
import Market
import Area
import TurnOrder
import Player
import Game


setupGame :: Updater Game ()
setupGame =
  do pnum <- with turnOrder (ask numberOfPlayers)
     mapM_ addVisTile $ map MarketId $ take pnum [ 0 .. ]

addVisTile :: MarketId -> Updater Game ()
addVisTile = addTile useVisible

addBlindTile :: MarketId -> Updater Game ()
addBlindTile = addTile useBlind

complete :: AreaId -> MarketId -> Updater Game ()
complete aid mid =
  do status <- ask gameStatus
     case status of
       NextTurn -> doComplete
       CompleteMarket aid1 mid1 mid2
         | aid == aid1 && (mid == mid1 || mid == mid2) -> doComplete
         | otherwise -> failure "This is not a valid market to complete."

       GameFinished -> failure "This game has finished."
       Promote {} -> failure "Some tiles still need to be promoted"

  where
  doComplete =
    do proms <- with (area aid) (completeMarketArea mid)
       case nextArea aid of
         Nothing ->
           do upd $ \g -> g { gamePalace = proms ++ gamePalace g }
              nextTurn
         Just aid1 ->
            do let todo t = PromoteTodo
                             { promTile = t
                             , promStartFrom = aid1
                             , promExclude = []
                             }
               autoPromote (map todo proms)

promote :: MarketId -> Updater Game ()
promote mid =
  do status <- ask gameStatus
     case status of
       Promote pt rest ->
         case Map.lookup mid (promoteMarkets pt) of
           Just a ->
             do with (area (promoteArea pt)) $ set a
                autoPromote rest
           Nothing -> failure "That's not a valid promotion choice."
       GameFinished      -> failure "This game has finished."
       NextTurn          -> failure "There is nothing to promote."
       CompleteMarket {} -> failure "A market still needs to be completed."

rejoin :: AreaId -> Tile -> MarketId -> Updater Game ()
rejoin aid t mid =
  do whenReady
     ot <- ask (owned t)
     with (area aid) $ do rmVagrantArea ot
                          addTileArea mid ot
     nextTurn


vagrantMoveSwap ::
  AreaId -> Tile -> GlobMarketId -> Tile -> MarketId -> Updater Game ()
vagrantMoveSwap vaid vt from t toMID =
  do whenReady
     -- spend vagrant
     vtile <- ask (owned vt)
     with (area vaid) (rmVagrantArea vtile)
     upd $ \g -> g { gameRemoved = vtile : gameRemoved g }

     -- move or swap tile
     let aid     = gmAID from
         fromMID = gmMID from
     tile <- ask (owned t)
     both <- with (area aid) (areaMoveSwap tile fromMID toMID)
     if both
        then upd $ \g -> g { gameStatus = CompleteMarket aid fromMID toMID }
        else complete aid toMID


vagrantJumpQueue :: AreaId -> Tile -> GlobMarketId -> Updater Game ()
vagrantJumpQueue aid t gmid =
  do whenReady
     ot <- ask (owned t)
     with (area aid) (rmVagrantArea ot)

     let tgt = gmMID gmid
     with (area (gmAID gmid)) $
       with (market tgt) (vagrantJumpMarket ot)

     upd $ \g -> g { gameRemoved = ot { tileType = nextTile (nextTile t) }
                                 : gameRemoved g }
     complete aid tgt


endGame :: Updater Game ()
endGame =
  do curPID  <- with turnOrder (ask curPlayer)
     noTiles <- with (player curPID) $ ask (null . playerStack)
     unless noTiles $ failure "You can't end the game if you still have tiles."
     mayRejoin <- ask (any (vagrantCanRejoin curPID) . gameAreas)
     when mayRejoin $ failure "You still have vagrants who can rejoin."
     upd $ \g -> g { gameStatus = GameFinished }




