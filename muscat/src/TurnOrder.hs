{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module TurnOrder where


import Util.Perhaps

import Updater
import Ids


data PlayerOrder  = PlayerOrder { playersDone :: [PlayerId]
                                , curPlayer   :: PlayerId
                                , nextPlayers :: [PlayerId]
                                , firstPlayer :: PlayerId
                                }

numberOfPlayers :: PlayerOrder -> Int
numberOfPlayers p = 1 + length (playersDone p) + length (nextPlayers p)

atRoundStart :: PlayerOrder -> Bool
atRoundStart o = curPlayer o == firstPlayer o

newPlayerOrder :: [PlayerId] -> Perhaps PlayerOrder
newPlayerOrder ps =
  case ps of
    [] -> Failed "We need at least one player."
    p : rest -> Ok PlayerOrder { playersDone = []
                               , curPlayer = p
                               , nextPlayers = rest
                               , firstPlayer = p }


advanceTurnOrder :: Updater PlayerOrder ()
advanceTurnOrder = upd $ \PlayerOrder { .. } ->
  case nextPlayers of
    x : xs -> PlayerOrder { playersDone = curPlayer : playersDone
                          , curPlayer = x
                          , nextPlayers = xs
                          , .. }
    [] -> case reverse (curPlayer : playersDone) of
            ~(x : xs) -> PlayerOrder { playersDone = []
                                     , curPlayer = x
                                     , nextPlayers = xs
                                     , .. }


