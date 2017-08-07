{-# Language TemplateHaskell #-}
module Game where

import Control.Lens(makeLenses, (^.), (.~), (%~), (%%~), failover)
import Control.Monad((<=<))

import Util.Bag

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
  { _playerId     :: PlayerId
  , _playerTokens :: Bag Token
  }

data CurPawn = CurPawn
  { _curPawn           :: Pawn
  , _curPawnLoc        :: PawnLoc
  , _curPawnPowerBoost :: Int
  }

$(makeLenses 'Player)
$(makeLenses 'Game)
$(makeLenses 'CurPlayer)
$(makeLenses 'CurPawn)


curPawnNew :: PawnLoc -> Pawn -> CurPawn
curPawnNew l p =
  CurPawn { _curPawn = p, _curPawnLoc = l, _curPawnPowerBoost = 0 }

curPlayerNew :: Player -> CurPlayer
curPlayerNew p = CurPlayer { _curPlayer = p, _curPlayerPawn = Nothing }

playerNew :: PlayerId -> Player
playerNew p = Player { _playerId = p, _playerTokens = bagEmpty }



-- | Power for the current pawn, including boosts.
curPawnPower :: CurPawn -> Int
curPawnPower c = c ^. curPawn . pawnPower  +  c ^. curPawnPowerBoost

--------------------------------------------------------------------------------

-- | End the current player's turn and go to the next player.
nextPlayer :: Game -> Game
nextPlayer g = upd g
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
                  ps -> nextPlayer
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



-- | Boost the power of the current player.
gameBoostPawn :: Game -> Maybe Game
gameBoostPawn =
  gamePlayerCur %%~ (
    curPlayer . playerTokens %%~ bagRemove 1 PowerBoost
     <=<
    failover (curPlayerPawn . traverse . curPawnPowerBoost) (+1)
  )






--------------------------------------------------------------------------------






{-
-- | Influence exeretd by the current pawn on the board.
curPawnTilePower :: CurPawn -> (Int, [TileLoc])
curPawnTilePower p = pawnTilePower (curPawnLoc p) p'
  where p' = (curPawn p) { pawnPower = curPawnPower p }
-}
