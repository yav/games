{-# Language OverloadedStrings #-}

module Replay
( ReplayLog(..)
, Move(..)
, Replay(..)
, replayGame
, playMove
, addReplayMove
, emptyReplay
) where

import Data.Text (Text)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS

import Turn(GameInit, turnSkip, turnPlayCard, newGame)
import Deck(Element)
import CardTypes(Location)
import Game(Game)
import GameMonad(Log, GameStatus(..), runGame, GameM)

data Move =
    PlayCard Element Int (Maybe Location)
    | SkipTurn

data ReplayLog = ReplayLog
    { initialState :: GameInit
    , moves :: [Move]
    -- ^ the moves made in the game, starting with the most
    -- recently made move
    }

data Replay = Replay
    { states :: [(Game, Move, Log)]
    -- ^ a state in the game, followed by a log describing
    -- a transition to the next state
    , outcome :: GameStatus Game
    -- ^ the eventual outcome of the game
    }

emptyReplay :: GameInit -> ReplayLog
emptyReplay gi =
    ReplayLog { initialState = gi
              , moves = [] }

addReplayMove :: Move -> ReplayLog -> ReplayLog
addReplayMove mv lg = lg { moves = mv : moves lg }

replayMoves :: Game -> [Move] -> [(Game, Move, Log)] -> Replay
replayMoves g mvs acc =
  case mvs of
    []           -> Replay { states = reverse acc, outcome = GameOn g }
    mv:moreMoves ->
      let (status, g', l) =  oneStep g mv
      in case status of
        GameOn _ ->
          let acc' = (g, mv, l):acc
          in replayMoves g' moreMoves acc'
        GameStopped st ->
          Replay { states = reverse acc, outcome = GameStopped st }

replayMoves' :: Game -> [Move] -> Replay
replayMoves' g [] = Replay { states = [], outcome = GameOn g }
replayMoves' g (mv:moreMoves) =
  let (status, g', l) = oneStep g mv
  in case status of
    GameOn _ ->
      let after = replayMoves' g' moreMoves
      in  after { states = (g, mv, l):states after }
    GameStopped st -> Replay { states = [], outcome = GameStopped st }

oneStep :: Game -> Move -> (GameStatus (), Game, Log)
oneStep g m = runGame g (playMove m)

playMove :: Move -> GameM ()
playMove m =
  case m of
    SkipTurn -> turnSkip
    PlayCard elt rank mbloc -> turnPlayCard elt rank mbloc

replayGame :: ReplayLog -> Replay
replayGame l =
  let initGame = newGame (initialState l)
  in replayMoves initGame (reverse (moves l)) []

--------------------------------------------------------------------------------

instance ToJSON Move where
    toJSON mv = case mv of
        SkipTurn -> JS.object [ tag "skipTurn" ]
        PlayCard elt rnk mbloc -> JS.object 
            [ tag "playCard" 
            , "element"  .= elt
            , "rank"     .= rnk
            , "location" .= mbloc 
            ]
        where tag x = "tag" .= (x :: Text)

instance ToJSON Replay where
    toJSON rply = JS.object 
        [ "outcome" .= outcome rply
        , "gameHistory" .= [(g, mv, lg []) | (g, mv, lg) <- states rply ]
        ]

