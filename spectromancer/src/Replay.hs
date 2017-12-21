{-# Language OverloadedStrings #-}

module Replay
( Move(..)
, Replay(..)
, emptyReplay
, addReplayMove
, playMove
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

data Replay = Replay
    { initialState :: GameInit
    , states :: [(Game, Move, Log)]
    -- ^ a state in the game, followed by a move and a log describing
    -- a transition to the next state
    , outcome :: GameStatus Game
    -- ^ the eventual outcome of the game
    }

emptyReplay :: GameInit -> Replay
emptyReplay gi =
    Replay { initialState = gi
           , states       = []
           , outcome      = GameOn (newGame gi)
           }

addReplayMove :: Move -> Replay -> Replay
addReplayMove mv r =
  case outcome r of
    GameOn g ->
      r { outcome = case newStatus' of
                      GameOn ()     -> GameOn newG
                      GameStopped s -> GameStopped s
        , states = (g,mv,newL) : states r
        }

      where (newStatus', newG, newL) = oneStep g mv

    GameStopped {} -> r

oneStep :: Game -> Move -> (GameStatus (), Game, Log)
oneStep g m = runGame g (playMove m)

playMove :: Move -> GameM ()
playMove m =
  case m of
    SkipTurn -> turnSkip
    PlayCard elt rank mbloc -> turnPlayCard elt rank mbloc


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

