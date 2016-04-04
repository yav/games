{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Elements.Game
import Elements.BasicTypes
import Elements.Land(defaultLandSetup,exportLand)
import Elements.Terrain(MapShape(..),Addr(..),HexAddr(..),Dir(..),hexAddr)
import Util.Random
import Util.JSON
import qualified Util.JSON as JS
import Util.Snap
import Util.History
import Util.Bag
import Util.Perhaps

import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory)
import           Control.Applicative ((<|>))
import           Control.Concurrent
import qualified Data.Map as Map
import           Data.Text ( Text )
import qualified Data.Text as Text
import           Data.Maybe(mapMaybe)


type S = MVar GameState

data GameState = GameState { theGame     :: History Game
                           , theQuestion :: Maybe ChoiceMap
                           }




main :: IO ()
main =
  do r <- randSourceIO
     g <- case newGame r (defaultLandSetup Wedge) of
            Ok g       -> return g
            Failed err -> fail (Text.unpack err)

     s <- newMVar GameState { theGame = history $ startTurn g
                            , theQuestion = Nothing
                            }
     quickHttpServe $ Snap.route
       [ ("/view", snapView s)
       , ("/answer", snapAnswer s)
       , ("/summon", snapSummon s)
       , ("/summonStored", snapSummonStored s)
       , ("/action", snapAction s)
       , ("/nextTurn", snapNextTurn s)
       , ("/move", snapMove s)
       ] <|> serveDirectory "ui"

snapView :: S -> Snap ()
snapView s =
  do js <- snapIO (withMVar s return)
     sendJSON js


snapWithGame :: S -> (Game -> Maybe Game) -> Snap ()
snapWithGame s upd =
  snapModifyMVar_ s $ \state ->
  case theQuestion state of
    Just _  -> badInput "Busy"
    Nothing ->
      case historyUpdateF upd (theGame state) of
        Just h -> do let s1 = state { theGame = h }
                     sendJSON s1
                     return s1
        Nothing -> badInput "Failed to perofrm command"

snapInteract :: S -> IM () -> Snap ()
snapInteract s i = snapModifyMVar_ s $ \state ->
  case theQuestion state of
    Nothing ->
      case runInteract (runIM i) (historyCurrent (theGame state)) of
        Right mp -> snapWhatNext mp state
        Left x   -> badInput (Text.unwords [ "Can't interact", Text.pack x ])
    Just _  -> badInput "Busy"

snapWhatNext :: ChoiceMap -> GameState -> Snap GameState
snapWhatNext mp state =
  do let s1 = case mp of
                NoChoice g ->
                  GameState { theGame = historyUpdate (const g) (theGame state)
                            , theQuestion = Nothing
                            }
                NumChoices g _ -> state { theGame = historyUpdate (const g) (theGame state), theQuestion = Just mp }
     sendJSON s1
     return s1

snapAnswer :: S -> Snap ()
snapAnswer s =
  snapModifyMVar_ s $ \state ->
    case theQuestion state of
      Nothing -> badInput "No question"
      Just mp ->
        case mp of
          NoChoice _ -> badInput "Impossibe NoChoice in continuation"
          NumChoices g m ->
            do p <- snapParam "answer"
               case Map.lookup p m of
                 Just (_,m1) -> snapWhatNext m1
                                   state { theGame = historyUpdate (const g)
                                                                 (theGame state) }
                 Nothing     -> badInput "Invalid answer"

snapSummon :: S -> Snap ()
snapSummon s =
  do sp <- snapParam "spirit"
     snapInteract s (summon sp)

snapSummonStored :: S -> Snap ()
snapSummonStored s =
  do sp <- snapParam "spirit"
     snapInteract s (summonFriendly sp)

snapNextTurn :: S -> Snap ()
snapNextTurn s = snapWithGame s (Just . startTurn . endTurn)

snapAction :: S -> Snap ()
snapAction s =
  do a     <- snapParam "action"
     which <- snapParam "type"
     case which :: Text of
       "basic"    -> snapInteract s (useBasic a) -- XXX: Var
       "advanced" -> snapInteract s (useAdvanced a) -- XXX: var
       _ -> badInput "Invalid actoin type"

snapMove :: S -> Snap ()
snapMove s =
  do a <- theAddrParam
     snapInteract s (move a)

--------------------------------------------------------------------------------

enumParam :: (Enumerate a, ExportAsKey a) => Text -> Text -> Snap a
enumParam msg p =
  do txt <- snapParam p
     case lookup txt [ (toKeyJS s, s) | s <- enumerate ] of
       Nothing -> badInput (Text.unwords [ "Invalid", msg, p ])
       Just sp -> return sp

instance SnapParam Element where
  snapParam = enumParam "element"

instance SnapParam Spirit where
  snapParam = enumParam "spirit"

instance SnapParam Action where
  snapParam = enumParam "action"

theAddrParam :: Snap Addr
theAddrParam =
  do x <- snapParam "tile_x"
     y <- snapParam "tile_y"
     h <- snapParam "hex"
     let ok d = return (hexAddr d)
     lo <- case h :: Text of
             "NW" -> ok NW
             "NE" -> ok NE
             "W"  -> ok W
             "C"  -> ok Center
             "E"  -> ok E
             "SW" -> ok SW
             "SE" -> ok SE
             _    -> badInput "Malformed parameter: hex"
     return Addr { addrGlobal = (x,y), addrLocal = lo }


--------------------------------------------------------------------------------

instance ExportAsKey Action where
  toKeyJS = actName

instance Export Action where
  toJS a = JS.object [ "name"     .= actName a
                     , "element"  .= actElement a
                     , "basic"    .= actHelp (actBase a)
                     , "advanced" .= actHelp (actAdvanced a)
                     ]

instance Export Description where
  toJS d = case d of
            Text t      -> JS.object [ jsTag "text", "val" .= t ]
            Image t     -> JS.object [ jsTag "image", "val" .= t ]
            ActionD a   -> JS.object [ jsTag "action", "val" .= a ]
            GlueHor x y -> JS.object [ jsTag "hor", "left" .= x, "right" .= y ]
            GlueVer x y -> JS.object [ jsTag "ver", "above" .= x, "below" .= y ]


exportResources :: Resources -> JS.Value
exportResources s = JS.object
  [ "points"   .= mapMaybe fld [ Movement, Block, Attack, Currency,
                                 Heal, CharacterFlaw ]

  , "trove"    .= JS.object [ "summon"  .= bagLookup Summon s
                            , "spirits" .= [ JS.object
                                               [ "spirit" .= x
                                               , "state"  .= t
                                               ] | Spirit x t <- list' ]
                            ]
  , "plane"    .= JS.object
                  [ "friendly" .= [ pair e a | (Friendly e,a) <- list ]
                  , "summoned" .= [ pair e a | (Summoned e,a) <- list ]
                  ]

  , "actions"  .= JS.object
                    [ "available" .= [ a | Action a     <- list' ]
                    , "used"      .= [ a | UsedAction a <- list' ]
                    ]
  ]
  where
  list     = bagToListGrouped s
  list'    = bagToList s
  pair x y = JS.object [ "type" .= x, "amount" .= y ]
  fld x    = case bagLookup x s of
               0 -> Nothing
               n -> Just (pair x n)

instance ExportAsKey SpiritState where
  toKeyJS a =
    case a of
      IsAvailable    -> "available"
      WasSummoned    -> "summoned"
      WasTransformed -> "transformed"

instance Export SpiritState where
  toJS = jsKey

instance Export Game where
  toJS g = JS.object
            [ "past"      .= gamePast g
            , "alignment" .= gameAlignment g
            , "limit"     .= gameActLimit g
            , "resources" .= exportResources (gameResources g)
            , "land"      .= exportLand (gameLocation g) (gameLand g)
            ]

instance Export ChoiceMap where
  toJS cm =
    case cm of
      NumChoices _ m -> JS.object [ Text.pack (show n) .= d
                                       | (n,(d,_)) <- Map.toList m ]
      NoChoice  _  -> error "Exporting NoChoice question"

instance Export GameState where
  toJS g = JS.object [ "game"     .= historyCurrent (theGame g)
                     , "question" .= theQuestion g
                     ]

instance Export Resource where
  toJS r =
    case r of
      Movement      -> txt "movement"
      Block         -> txt "block"
      Attack        -> txt "attack"
      Currency      -> txt "currency"
      Heal          -> txt "heal"
      CharacterFlaw -> txt "flaw"
      Summon        -> txt "summon"
      Spirit s st   -> JS.object [ "spirit" .= s, "state" .= st ]
      Friendly e    -> JS.object [ "friendly" .= e ]
      Summoned e    -> JS.object [ "summoned" .= e ]
      Action a      -> JS.object [ "action" .= a ]
      UsedAction a  -> JS.object [ "used_action" .= a ]
    where
    txt x = toJS (x :: Text)
