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
              GameInit {- ^ Initial state of the game -} ->
              Game {- ^ Final state of the game -} ->
              FinishedGame

finishGame gWin gi gEnd = FinishedGame
  { player1    = p1Name
  , class1     = p1Class
  , player2    = p2Name
  , class2     = p2Class
  , winnerName = gEnd ^. player gWin . playerName
  , winner     = gWin
  }
  where
  (p1Name,p1Class) = firstPlayer gi
  (p2Name,p2Class) = secondPlayer gi


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



