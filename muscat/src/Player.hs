{-# Language OverloadedStrings #-}
module Player (Player, newPlayer, hasTiles, useVisible, useBlind) where

import Data.Text(Text)
import Util.Random

import Updater
import Tile

data Player       = Player { playerName    :: Text
                           , playerStack   :: [Tile] -- top one is visible
                           }

hasTiles :: Player -> Bool
hasTiles = not . null . playerStack

useVisible :: Updater Player Tile
useVisible =
  do stack <- ask playerStack
     case stack of
       t : ts -> do upd $ \p -> p { playerStack = ts }
                    pure t
       []     -> failure "This player has no more tiles."


useBlind :: Updater Player Tile
useBlind =
  do stack <- ask playerStack
     case stack of
       v : t : more -> do upd $ \p -> p { playerStack = v : more }
                          pure t
       _  -> failure "This player has no unrevealed tiles."

newPlayer :: Text -> Gen Player
newPlayer nm =
  do ts <- shuffle tileSet
     pure Player { playerName = nm, playerStack = ts }



