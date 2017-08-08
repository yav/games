{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JSON where

import Control.Lens((^.))
import qualified Data.Aeson as JS
import Data.Aeson (ToJSON(..), (.=))
import           Data.Map ( Map )
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Util.Bag

import Basics
import Board
import Game

instance ToJSON Pawn where
  toJSON p = JS.object [ "player" .= (p ^. pawnPlayer)
                       , "power"  .= (p ^. pawnPower)
                       , "speed"  .= (p ^. pawnSpeed)
                       ]

instance ToJSON Board where
  toJSON b = JS.object [ "tiles" .= jsTiles (b ^. boardTiles)
                       , "pawns" .= jsPawns (b ^. boardPawns)
                       ]

-- | Assumes only non-negative coordinates.
jsTiles :: Map TileLoc Tile -> JS.Value
jsTiles m = toJSON [ [ Map.lookup (x,y) m
                     | x <- take width  [ 0 .. ]
                     ]
                   |  y <- reverse $ take height [ 0 .. ]
                   ]
  where
  (xLocs,yLocs) = unzip (Map.keys m)
  width         = maximum ((-1) : xLocs) + 1
  height        = maximum ((-1) : yLocs) + 1

instance ToJSON Tile where
  toJSON = toJSON . show  -- XXX?

jsPawns :: Pawns -> JS.Value
jsPawns m = JS.object [ jsPawnLoc l .= ps | (l,ps) <- Map.toList m ]

jsPawnLoc :: PawnLoc -> Text
jsPawnLoc (PawnLoc x y) = Text.pack (show x ++ "_" ++ show y)

instance ToJSON Player where
  toJSON p = JS.object [ "id"         .= (p ^. playerId)
                       , "workers"    .= (p ^. playerWorkers)
                       , "powerBoost" .= tok PowerBoost
                       , "speedBoost" .= tok SpeedBoost
                       , "shareSpace" .= tok ShareSpace
                       ]
    where tok x = bagLookup x (p ^. playerTokens)

instance ToJSON CurPawn where
  toJSON cp = JS.object [ "pawn"  .= (cp ^. curPawn)
                        , "loc"   .= jsPawnLoc (cp ^. curPawnLoc)
                        , "boost" .= (cp ^. curPawnPowerBoost)
                        , "move"  .= (cp ^. curPawnMovement)
                        ]

instance ToJSON CurPlayer where
  toJSON p = JS.object [ "pawn"   .= (p ^. curPlayerPawn)
                       , "player" .= (p ^. curPlayer)
                       ]

instance ToJSON Game where
  toJSON g = JS.object [ "board"   .= (g ^. gameBoard)
                       , "player"  .= (g ^. gamePlayerCur)
                       , "players" .= ( (g ^. gamePlayersNext) ++
                                        reverse (g ^. gamePlayersPrev))
                       ]


