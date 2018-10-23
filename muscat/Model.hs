{-# Language OverloadedStrings #-}
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set


import Util.Perhaps

tileNum :: Int
tileNum = 4

areaNum :: Int
areaNum = 4

winnerNum :: Int
winnerNum = ceiling (fromIntegral (tileNum - 1) / 2 :: Float)

marketsPerArea :: Int -> Int
marketsPerArea p
  | p <= 3    = 3
  | otherwise = p

promotedEnds :: Int -> Int
promotedEnds p = 4 + 2 * div p 2

newtype Tile      = Tile Int
                    deriving (Eq,Ord,Show)

ppTile :: Tile -> Text
ppTile (Tile x) = " a " <> Text.pack (show x) <> " tile"

newtype PlayerId  = PlayerId Int deriving (Eq,Ord)
newtype AreaId    = AreaId   Int deriving (Eq,Ord)
newtype MarketId  = MarketId Int deriving (Eq,Ord)

data Player       = Player { playerName    :: Text
                           , playerStack   :: [Tile] -- top one is visible
                           }

useVisible :: Player -> Perhaps (Tile,Player)
useVisible p =
  case playerStack p of
    [] -> Failed "This player has no more tiles."
    t : ts -> Ok (t, p { playerStack = ts })

useBlind :: Player -> Perhaps (Tile, Player)
useBlind p =
  case playerStack p of
    v : t : more -> Ok (t, p { playerStack = v : more })
    _  -> Failed "This player has no unrevealed tiles."


data Market       = Market { marketHas     :: Map Tile PlayerId
                           , marketMissing :: Set Tile
                           }

data OwnedTile    = OwnedTile { tileType :: Tile, tileOwner :: PlayerId }

emptyMarket :: Market
emptyMarket =
  Market { marketHas = Map.empty
         , marketMissing = Set.fromList $ map Tile $ take tileNum [ 0 .. ]
         }

addTileMarket :: OwnedTile -> Market -> Perhaps Market
addTileMarket ot m
  | Set.size (marketMissing m) == 1 =
    Failed "Market is full"

  | t `Map.member` (marketHas m) =
    Failed ("Market already conatins " <> ppTile t)

  | otherwise =
    Ok Market { marketHas     = Map.insert t (tileOwner ot) (marketHas m)
              , marketMissing = Set.delete t (marketMissing m)
              }
  where t = tileType ot

completeMarket :: Market -> Perhaps ([OwnedTile],[OwnedTile])
completeMarket m =
  case Set.minView (marketMissing m) of
    Just (a,bs)
      | Set.null bs -> Ok $ splitAt winnerNum
                          $ map toOwnedTile
                          $ Map.toList sooner ++ Map.toList later
      where
      (later,sooner)    = Map.split a (marketHas m)
      toOwnedTile (t,p) = OwnedTile { tileType = t, tileOwner = p }

    _ -> Failed "Market is not yet full."


--------------------------------------------------------------------------------
data Area         = Area { areaMarkets :: Map MarketId Market
                         , areaStreet  :: [OwnedTile]
                         }

newArea :: Int -> Area
newArea n = Area { areaMarkets = Map.fromList [ (MarketId i,emptyMarket)
                                                    | i <- take n [ 0 .. ] ]
                 , areaStreet = [] }

withMarket :: MarketId -> Area -> (Market -> Perhaps a) -> Perhaps a
withMarket mid a f =
  case Map.lookup mid (areaMarkets a) of
   Nothing -> Failed "There is no such market."
   Just m  -> f m

updMarket :: MarketId -> (Market -> Perhaps Market) -> Area -> Perhaps Area
updMarket mid f a =
  withMarket mid a $ \m ->
  do m1 <- f m
     pure a { areaMarkets = Map.insert mid m1 (areaMarkets a) }

addTileArea :: MarketId -> OwnedTile -> Area -> Perhaps Area
addTileArea mid ot = updMarket mid (addTileMarket ot)

completeMarketArea :: MarketId -> Area -> Perhaps ([OwnedTile],Area)
completeMarketArea mid a =
  withMarket mid a $ \m ->
    do (win,loose) <- completeMarket m
       pure (win, a { areaMarkets = Map.insert mid emptyMarket (areaMarkets a)
                    , areaStreet = loose ++ areaStreet a
                    })

autoPromoteArea :: OwnedTile -> Area -> [(MarketId,Area)]
autoPromoteArea ot a =
  [ (mid, a1) | mid   <- Map.keys (areaMarkets a)
              , Ok a1 <- [ addTileArea mid ot a ]
              ]

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
data Game         = Game { gameAreas    :: Map AreaId Area
                         , gamePlayers  :: Map PlayerId Player
                         , gamePalace   :: [OwnedTile]
                         , gameOrder    :: PlayerOrder
                         , gameRemoved  :: [OwnedTile]
                         , gameStatus   :: GameStatus
                         }

firstArea :: AreaId
firstArea = AreaId 0

nextArea :: AreaId -> Maybe AreaId
nextArea (AreaId x)
  | y < x       = Just (AreaId y)
  | otherwise   = Nothing
  where y = x + 1

withArea :: AreaId -> Game -> (Area -> Perhaps a) -> Perhaps a
withArea aid g f =
  case Map.lookup aid (gameAreas g) of
    Nothing -> Failed "There is no such area."
    Just a  -> f a

withCurPlayer :: Game -> (PlayerId -> Player -> Perhaps a) -> Perhaps a
withCurPlayer g f =
  case Map.lookup pid (gamePlayers g) of
    Just p  -> f pid p
    Nothing -> Failed "[bug] Invalid current player."
  where
  pid = curPlayer (gameOrder g)

updArea :: AreaId -> Game -> (Area -> Perhaps Area) -> Perhaps Game
updArea aid g f = withArea aid g $ \a ->
  do a1 <- f a
     pure g { gameAreas = Map.insert aid a1 (gameAreas g) }

whenReady :: Game -> Perhaps ()
whenReady g =
  case gameStatus g of
    NextTurn     -> pure ()
    GameFinished -> Failed "This game has finished."
    Promote {}   -> Failed "Some tiles still need to be promoted."

autoPromote :: Game -> [(AreaId,OwnedTile)] -> Perhaps Game
autoPromote g ots =
  case ots of
    [] -> pure g
    (aid,ot) : rest ->
      withArea aid g $ \a ->
        case autoPromoteArea ot a of
          [] ->
            case nextArea aid of
              Nothing -> pure g { gamePalace = ot : gamePalace g }
              Just aid1 -> autoPromote g ((aid1,ot):rest)

          [(_,a1)] ->
            let g1 = g { gameAreas = Map.insert aid a1 (gameAreas g) }
            in autoPromote g1 rest

          opts ->
            let tgt = PromotionTarget
                        { promoteArea    = aid
                        , promoteMarkets = Map.fromList opts
                        , promoteTile    = ot }
            in pure g { gameStatus = Promote tgt rest }

nextTurn :: Game -> Game
nextTurn g
  | curPlayer order == firstPlayer order = gameFinished g1
  | otherwise                            = g1
  where
  order = advanceTurnOrder (gameOrder g)
  g1    = g { gameOrder = order }

addThisTile :: MarketId -> OwnedTile -> Game -> Perhaps Game
addThisTile mid ot g = whenReady g >> updArea firstArea g (addTileArea mid ot)

addTile :: (Player -> Perhaps (Tile,Player)) -> MarketId -> Game -> Perhaps Game
addTile how mid g =
  withCurPlayer g $ \pid p ->
    do (t,p1) <- how p
       let g1 = g { gamePlayers = Map.insert pid p1 (gamePlayers g) }
           ot = OwnedTile { tileType = t, tileOwner = pid }
       nextTurn <$> addThisTile mid ot g1


gameFinished :: Game -> Game
gameFinished g
  | length (gamePalace g) >= promotedEnds (Map.size (gamePlayers g)) =
    g { gameStatus = GameFinished }
  | otherwise = g

--------------------------------------------------------------------------------
-- Entry Points

addVisTile :: MarketId -> Game -> Perhaps Game
addVisTile = addTile useVisible

addBlindTile :: MarketId -> Game -> Perhaps Game
addBlindTile = addTile useBlind

complete :: AreaId -> MarketId -> Game -> Perhaps Game
complete aid mid g =
  do whenReady g
     g1 <- withArea aid g $ \a ->
           do (proms,a1) <- completeMarketArea mid a
              let g1 = g { gameAreas = Map.insert aid a1 (gameAreas g) }
              case nextArea aid of
                Nothing -> pure g1 { gamePalace = proms ++ gamePalace g1 }
                Just aid1 -> autoPromote g1 (zip (repeat aid1) proms)
     pure (nextTurn g1)

promote :: MarketId -> Game -> Perhaps Game
promote mid g =
  case gameStatus g of
    Promote pt rest ->
      case Map.lookup mid (promoteMarkets pt) of
        Just a -> autoPromote g1 rest
          where
          g1 = g { gameAreas  = Map.insert (promoteArea pt) a (gameAreas g)
                 , gameStatus = NextTurn }

        Nothing -> Failed "That's not a valid promotion choice."
    GameFinished -> Failed "This game has finished."
    NextTurn -> Failed "There is nothing to promote."


--------------------------------------------------------------------------------
data PlayerOrder  = PlayerOrder { playersDone :: [PlayerId]
                                , curPlayer   :: PlayerId
                                , nextPlayers :: [PlayerId]
                                , firstPlayer :: PlayerId
                                }

advanceTurnOrder :: PlayerOrder -> PlayerOrder
advanceTurnOrder o =
  case nextPlayers o of
    x : xs -> o { playersDone = curPlayer o : playersDone o
                , curPlayer = x
                , nextPlayers = xs }
    [] -> case reverse (curPlayer o : playersDone o) of
            ~(x : xs) -> o { playersDone = []
                           , curPlayer = x
                           , nextPlayers = xs }
--------------------------------------------------------------------------------

data GameStatus = NextTurn
                | GameFinished
                | Promote PromotionTarget [(AreaId,OwnedTile)]

data PromotionTarget = PromotionTarget
  { promoteArea     :: AreaId
  , promoteMarkets  :: Map MarketId Area
  , promoteTile     :: OwnedTile
  }







