module GameStats
  ( FinishedGame(..)
  , finishGame
  ) where

import Data.Text(Text)
import Control.Lens((^.))

import Game
import CardTypes(Who(..))
import Deck(Class)

gameStatsVersion :: Int
gameStatsVersion = 1

finishGame :: Who  {- ^ Who won the game -} ->
              Game {- ^ Initial state of the game -} ->
              Game {- ^ Final state of the game -} ->
              FinishedGame

finishGame gWin gStart gEnd = FinishedGame
  { player1    = gStart ^. p1 . playerName
  , class1     = gStart ^. p1 . playerClass
  , player2    = gStart ^. p2 . playerName
  , class2     = gStart ^. p2 . playerClass
  , winnerName = gEnd   ^. player gWin . playerName
  , winner     = gWin
  }
  where
  p1 = player Caster
  p2 = player Opponent


data FinishedGame = FinishedGame
  { player1, player2 :: Text
  , class1, class2   :: Class
  , winnerName       :: Text
  , winner           :: Who
  } deriving (Read,Show)


parseFinishedGames :: String -> [FinishedGame]
parseFinishedGames inp =
  case reads inp of
    [] -> []
    (x,rest) : _ -> x : parseFinishedGames rest

loadFinishedGames :: FilePath -> IO [FinishedGame]
loadFinishedGames file = parseFinishedGames <$> readFile file



