{-# Language OverloadedStrings, RecordWildCards #-}
module Player where

import Data.Text
import Util.Random

import Updater
import Tile

data Player       = Player { playerName    :: Text
                           , playerStack   :: [Tile] -- top one is visible
                           }

useVisible :: Updater Player Tile
useVisible =
  do Player { .. } <- get
     case playerStack of
       t : ts -> do set Player { playerStack = ts, .. }
                    pure t
       [] -> failure "This player has no more tiles."

useBlind :: Updater Player Tile
useBlind =
  do Player {..} <- get
     case playerStack of
       v : t : more -> do set Player { playerStack = v : more, .. }
                          pure t
       _  -> failure "This player has no unrevealed tiles."

newPlayer :: Text -> Gen Player
newPlayer nm =
  do ts <- shuffle tileSet
     pure Player { playerName = nm, playerStack = ts }



