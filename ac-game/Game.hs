{-# Language TemplateHaskell, OverloadedStrings #-}
module Game where

import Control.Lens(makeLenses, (^.), (.~), (%~), (%%~), failover, (&), mapped)
import Control.Lens((+~),ix, (^?))
import Control.Monad(guard)
import Data.Maybe(mapMaybe)
import Data.Map(Map)

import Util.Bag
import Util.Perhaps

import Basics
import Board

data Game = Game
  { _gamePlayersPrev :: [Player]
  , _gamePlayerCur   :: CurPlayer
  , _gamePlayersNext :: [Player]
  , _gameBoard       :: Board
  }

data CurPlayer = CurPlayer
  { _curPlayer     :: Player
  , _curPlayerPawn :: Maybe CurPawn
  }

data Player = Player
  { _playerId       :: PlayerId
  , _playerTokens   :: Bag Token
  , _playerWorkers  :: Int -- ^ Unallocated basic workers
  }

data CurPawn = CurPawn
  { _curPawn           :: Pawn
  , _curPawnLoc        :: PawnLoc
  , _curPawnPowerBoost :: Int       -- ^ Additional power from tokens
  , _curPawnMovement   :: Int       -- ^ Remaining movement points
  }

$(makeLenses 'Player)
$(makeLenses 'Game)
$(makeLenses 'CurPlayer)
$(makeLenses 'CurPawn)


curPawnNew :: PawnLoc -> Pawn -> CurPawn
curPawnNew l p =
  CurPawn { _curPawn    = p
          , _curPawnLoc = l
          , _curPawnPowerBoost = 0
          , _curPawnMovement = p ^. pawnSpeed
          }

curPlayerNew :: Player -> CurPlayer
curPlayerNew p = CurPlayer { _curPlayer = p, _curPlayerPawn = Nothing }

playerNew :: PlayerId -> Player
playerNew p = Player { _playerId = p
                     , _playerTokens = bagEmpty
                     , _playerWorkers = 2 }




--------------------------------------------------------------------------------

gameNew :: Int -> Map TileLoc Tile -> Perhaps Game
gameNew n mp
  | n < 1{-prolly 2-} = fail "Invlid player count."
  | otherwise =
    return Game { _gamePlayersPrev = []
                , _gamePlayerCur   = curPlayerNew p
                , _gamePlayersNext = ps
                , _gameBoard       = boardNew mp
                }
  where p : ps = map playerNew (take n [ 0 .. ])




-- | End the current player's turn and go to the next player.
gameNextPlayer :: Game -> Game
gameNextPlayer g = upd g
  where
  upd = case g ^. gamePlayersNext of

          p : ps -> (gamePlayersPrev %~ (player ^. curPlayer :))
                  . (gamePlayersNext .~ ps)
                  . (gamePlayerCur   .~ curPlayerNew p)
                  . (gameBoard       %~ newBoard)

          [] -> case g ^. gamePlayersPrev of

                  -- Single plyer
                  [] -> (gamePlayerCur .~ curPlayerNew (player ^. curPlayer))
                      . (gameBoard     %~ newBoard)

                  -- Swap the queues
                  ps -> gameNextPlayer
                      . (gamePlayersNext .~ reverse ps)
                      . (gamePlayersPrev .~ [])

  player = g ^. gamePlayerCur

  -- return the old current player's token back to the board
  newBoard  = case player ^. curPlayerPawn of
                Nothing -> id
                Just cp -> boardPawns %~ addPawn (cp^.curPawnLoc) (cp^.curPawn)



-- | The current player picks a pawn
gameSelectPawn :: PawnLoc -> Pawn -> Game -> Maybe Game
gameSelectPawn l p g =
  case g ^. gamePlayerCur . curPlayerPawn of
    Just _  -> Nothing -- already selected
    Nothing -> (gamePlayerCur.curPlayerPawn .~ Just (curPawnNew l p))
           <$> (gameBoard.boardPawns) (rmPawn l p) g

-- | Place one of the unallocated workers on the board.
gamePlaceNewWorker :: PawnLoc -> Game -> Perhaps Game
gamePlaceNewWorker l g = g & (gamePlayerCur %%~ upd)
                           . (gameBoard . boardPawns %~ newPawn l pid)
  where
  pid = g ^. gamePlayerCur . curPlayer . playerId
  upd cp =
    do let decWorker n = guard (n > 0) >> return (n - 1)
       cp1 <- perhaps "Insufficient workers"
                $ cp & curPlayer . playerWorkers %%~ decWorker

       gameEnterLocation g l cp1





-- | Boost the power of the current player.
gameBoostPawn :: Game -> Perhaps Game
gameBoostPawn = gamePlayerCur %%~ upd
  where
  upd cp =
    do cp1 <- perhaps "Insufficient boost power"
                $ cp & curPlayer . playerTokens %%~ bagRemove 1 PowerBoost
       perhaps "No pawn is selected."
          $ cp1 & failover (curPlayerPawn . traverse . curPawnPowerBoost) (+1)


-- | Give a token to the current player.
gameAddToken :: Token -> Game -> Game
gameAddToken t = gamePlayerCur . curPlayer . playerTokens %~ bagAdd 1 t

-- | Move the current token to the given location, which should
-- be adjacent to the current location.
gameMove :: PawnLoc -> Game -> Perhaps Game
gameMove l g = g & gamePlayerCur %%~ upd
  where
  upd cp =
    do pawn <- perhaps "No pawn is selected." (cp ^. curPlayerPawn)

       checkThat (pawnDistance (pawn ^. curPawnLoc) l == 1)
                 "Movement can only happen one step at a time."

       cp1 <- perhaps "Insufficient movement points"
          $ if pawn ^. curPawnMovement == 0
              then cp & curPlayer . playerTokens %%~ bagRemove 1 SpeedBoost
              else let pawn1 = pawn & curPawnMovement %~ subtract 1
                   in return $ cp & curPlayerPawn .~ Just pawn1

       cp2 <- gameEnterLocation g l cp1
       return (cp2 & curPlayerPawn . mapped . curPawnLoc .~ l)

-- | Place a pawn on the given location.
-- Checks that the space is on the board, and if it is occupied, that
-- the player can pay the necessary cost.
gameEnterLocation :: Game -> PawnLoc -> CurPlayer -> Perhaps CurPlayer
gameEnterLocation g l cp =
  do checkThat (boardHasLoc (g ^. gameBoard) l)
        "The location is outside the board."
     let owner = boardLocOwner l (g ^. gameBoard)
         blocked = maybe False (/= cp ^. curPlayer . playerId) owner
     if blocked
       then perhaps "The space is occupied" $
                     cp & curPlayer . playerTokens %%~ bagRemove 1 ShareSpace
       else return cp




-- | Get the actions (with their powers) for the current player,
-- together with an overall influence map.
gameAction :: Game -> ([(Int,Tile)], InfluenceMap)
gameAction g =
  case player ^. curPlayerPawn of
    Nothing -> ([], baseInf)
    Just pawn ->
      let p      = (pawn ^. curPawn) & pawnPower +~ (pawn ^. curPawnPowerBoost)
          (i,ts) = pawnTilePower (pawn ^. curPawnLoc) p
          infMap = foldr (addInfluence pid i) baseInf ts
          getAct tl = do act <- g ^? gameBoard . boardTiles . ix (tl :: TileLoc)
                         (pid',pwr) <- infMap ^? ix tl
                         guard (pid == pid')
                         return (pwr,act)
      in (mapMaybe getAct ts, infMap)

  where
  player  = g ^. gamePlayerCur
  pid     = player ^. curPlayer . playerId
  baseInf = influenceMap (g ^. gameBoard ^. boardPawns)


