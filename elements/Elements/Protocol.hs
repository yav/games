module Elements.Protocol where

import qualified Utils.JSON as JS
import           Data.Text(Text)

class Export t where
  toJS :: t -> JS.Value

class ExportAsKey t where
  toKeyJS :: t -> Text

(.=) :: Export t => Text -> t -> (Text, JS.Value)
x .= y = (x, toJS y)


instance ExportAsKey Element where
  toKeyJS e =
    case e of
      Earth -> "earth"
      Water -> "water"
      Fire  -> "fire"
      Air   -> "air"

instance Export Element where
  toJS = JS.text . jsKey

instance ExportAsKey Spirit where
  toKeyJS p = case p of
                Death     -> "death"
                Birth     -> "birth"
                Element e -> toKeyJS e

instance Export Spirit where
  toJS = JS.text jsKey

instance ExportAsKey EnemyType where
  toKeyJS et =
    case et of
      Orc         -> "orc"
      Guardian    -> "guardian"
      Mage        -> "mage"
      Underworld  -> "underworld"
      Citizen     -> "citizen"
      Draconum    -> "draconum"

instance Export EnemyType where
  toJS = JS.text . toKeyJS

--------------------------------------------------------------------------------
-- External format


instance Export Dir where
  toJS dir = JS.text txt
    where
    txt = case dir of
            NE -> "NE"
            E  -> "E"
            SE -> "SE"
            SW -> "SW"
            W  -> "W"
            NW -> "NW"


instance Export MapShape where
  toJS sh = case sh of
              Wedge           -> object [ "shape" .= "wedge" :: Text) ]
              OpenMap up down -> object [ "shape" .= ("open" :: Text)
                                        , "up"    .= up
                                        , "down"  .= down
                                        ]

instance Export Addr where
  toJS Addr { addrGlobal = (x,y), .. } = object [ "x"   .= x
                                                , "y"   .= y
                                                , "hex" .= addrLocal
                                                ]


instance Export HexAddr where
  toJS addr = case addr of
                Center   -> toJS ("C" :: Text)
                Border b -> toJS b

instance Export TileType where
  toJS t = toJS (txt :: Text)
    where
    txt = case t of
            BasicTile    -> "country"
            AdvancedTile -> "advanced"

instance Export Tile where
  toJS Tile { .. } =
    toJS [ object [ "addr"    .= addr
                  , "content" .= tileTerrain addr
                  ] | addr <- allHexAddrs ]

instance Export HexLandInfo where
  toJS HexLandInfo { .. } =
    object [ "terrain"  .= hexTerrain
           , "features" .= hexFeatures
           ]



instance Export EnemyVis where
  toJS v =
    case v of
      EnemyVisible -> "visible"
      EnemyHidden  -> "hidden"
      EnemyAmbush  -> "ambush"

instance Export Terrain where
  toJS t = case t of
             Road       -> txt "road"
             Hills      -> txt "hills"
             Forest     -> txt "forest"
             Lava       -> txt "lava"
             Desert     -> txt "desert"
             Bog        -> txt "bog"
             Sea        -> txt "sea"
             Mountain   -> txt "mountains"
             Empty      -> txt "empty"

    where txt x = toJS (x :: Text)

instance ExportAsKey Feature where
  toKeyJS f = case f of
                Shop   -> "shop"
                Castle -> "castle"
                Rest s -> "rest_" `Text.append` toKeyJS s

instance Export Feature where
  toJS = jsKey



exportGameTile :: [(Text,HexAddr)] -> GameTile -> JS.Value
exportGameTile ps GameTile { .. } =
  JS.object [ "static"  .= gameTile
            , "dynamic" .= map export (Map.toList gameTileContent)
            ]
    where
    players l = [ JS.object [ "name" .= name ] | (name,loc) <- ps, loc == l ]

    export (l,c) =
      JS.object
        [ "addr" .= l
        , "content"  .=
             JS.object
               [ "players" .= players l
               , "enemies" .= map enemy (hexEnemies c)
               ]
        ]

    enemy (vis,e) = object [ "visibility" .= vis, "enemy" .= mbName vis e ]
    mbName vis e  = case vis of
                      EnemyVisible -> Just (enemyName e)
                      _            -> Nothing


