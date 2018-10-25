{-# Language OverloadedStrings #-}
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set


import Util.Perhaps

--------------------------------------------------------------------------------
-- Configuration

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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Identifying things

newtype PlayerId  = PlayerId Int deriving (Eq,Ord)

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Tiles

newtype Tile      = Tile Int
                    deriving (Eq,Ord,Show)

ppTile :: Tile -> Text
ppTile (Tile x) = " a " <> Text.pack (show x) <> " tile"


data OwnedTile    = OwnedTile { tileType :: Tile, tileOwner :: PlayerId }
                    deriving Eq
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Players

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
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Markets

data Market       = Market { marketHas     :: Map Tile PlayerId
                           , marketMissing :: Set Tile
                           }


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




--------------------------------------------------------------------------------
-- Areas

data Area         = Area { areaMarkets :: Map MarketId Market
                         , areaStreet  :: [OwnedTile]
                         }

newtype MarketId  = MarketId Int deriving (Eq,Ord)


newArea :: Int -> Area
newArea n = Area { areaMarkets = markets
                 , areaStreet  = [] }
  where
  markets = Map.fromList [ (MarketId i,emptyMarket) | i <- take n [ 0 .. ] ]



withMarket :: MarketId -> Area -> (Market -> Perhaps a) -> Perhaps a
withMarket mid a f =
  case Map.lookup mid (areaMarkets a) of
   Nothing -> Failed "There is no such market."
   Just m  -> f m

addTileArea :: MarketId -> OwnedTile -> Area -> Perhaps Area
addTileArea mid ot a =
  withMarket mid a $ \m ->
  do m1 <- addTileMarket ot m
     pure a { areaMarkets = Map.insert mid m1 (areaMarkets a) }

completeMarketArea :: MarketId -> Area -> Perhaps ([OwnedTile],Area)
completeMarketArea mid a =
  withMarket mid a $ \m ->
  do (win,loose) <- completeMarket m
     pure (win, a { areaMarkets = Map.insert mid emptyMarket (areaMarkets a)
                  , areaStreet = loose ++ areaStreet a })

autoPromoteArea :: [MarketId] -> OwnedTile -> Area -> [(MarketId,Area)]
autoPromoteArea excluded ot a =
  [ (mid,a1) | mid   <- Map.keys (areaMarkets a)
             , not (mid `elem` excluded)
             , Ok a1 <- [ addTileArea mid ot a ]
             ]

rmVagrant :: OwnedTile -> Area -> Perhaps Area
rmVagrant ot a = case break (ot ==) (areaStreet a) of
                   (xs,_:ys) -> Ok a { areaStreet = xs ++ ys }
                   _ -> Failed "Area does not have the required vagrant."

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Turn order

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

atRoundStart :: PlayerOrder -> Bool
atRoundStart o = curPlayer o == firstPlayer o
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Promoting things

data PromotionTarget = PromotionTarget
  { promoteArea     :: AreaId
  , promoteMarkets  :: Map MarketId Area
  , promoteTile     :: OwnedTile
  }

data GlobMarketId = GlobMarketId { gmAID :: AreaId, gmMID :: MarketId }
                      deriving (Eq,Ord)

data PromoteTodo = PromoteTodo
  { promExclude   :: [GlobMarketId]
  , promStartFrom :: AreaId
  , promTile      :: OwnedTile
  }

promExcludeStart :: PromoteTodo -> [ MarketId ]
promExcludeStart p = [ gmMID gmid | gmid <- promExclude p
                                  , gmAID gmid /= promStartFrom p ]






--------------------------------------------------------------------------------
-- Game

data Game         = Game { gameAreas    :: Map AreaId Area
                         , gamePlayers  :: Map PlayerId Player
                         , gamePalace   :: [OwnedTile]
                         , gameOrder    :: PlayerOrder
                         , gameRemoved  :: [OwnedTile]
                         , gameStatus   :: GameStatus
                         }

newtype AreaId    = AreaId   Int deriving (Eq,Ord)

data GameStatus   = NextTurn
                  | GameFinished
                  | Promote PromotionTarget [PromoteTodo]
                  | CompleteMarket AreaId MarketId MarketId



firstArea :: AreaId
firstArea = AreaId 0

nextArea :: AreaId -> Maybe AreaId
nextArea (AreaId x)
  | y < areaNum = Just (AreaId y)
  | otherwise   = Nothing
  where y = x + 1



withArea :: AreaId -> Game -> (Area -> Perhaps a) -> Perhaps a
withArea aid g f =
  case Map.lookup aid (gameAreas g) of
    Nothing -> Failed "There is no such area."
    Just a  -> f a

gameCurPlayerId :: Game -> PlayerId
gameCurPlayerId = curPlayer . gameOrder

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
    CompleteMarket {} -> Failed "You need to choose which market to promote."

autoPromote :: Game -> [PromoteTodo] -> Perhaps Game
autoPromote g ots =
  case ots of
    [] -> pure g
    todo : rest ->
      let aid = promStartFrom todo
          ot  = promTile todo
      in
      withArea aid g $ \a ->
        case autoPromoteArea (promExcludeStart todo) ot a of
          [] ->
            case nextArea aid of
              Nothing -> pure g { gamePalace = ot : gamePalace g }
              Just aid1 -> autoPromote g (todo { promStartFrom = aid1 }:rest)

          [(mid,a1)] ->
            let g1 = g { gameAreas = Map.insert aid a1 (gameAreas g) }
                gmid = GlobMarketId { gmAID = aid, gmMID = mid }
                avoid t = t { promExclude = gmid : promExclude t }
            in autoPromote g1 (map avoid rest)

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
addThisTile mid ot g =
  do whenReady g
     updArea firstArea g (addTileArea mid ot)

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
  case gameStatus g of
    NextTurn -> doComplete
    CompleteMarket aid1 mid1 mid2
      | aid == aid1 && (mid == mid1 || mid == mid2) -> doComplete
      | otherwise -> Failed "This is not a valid market to complete."

    GameFinished -> Failed "This game has finished."
    Promote {} -> Failed "Some tiles still need to be promoted"

  where
  doComplete =
    do g1 <- withArea aid g $ \a ->
          do (proms,a1) <- completeMarketArea mid a
             let g1 = g { gameAreas = Map.insert aid a1 (gameAreas g) }
             case nextArea aid of
               Nothing -> pure g1 { gamePalace = proms ++ gamePalace g1 }
               Just aid1 ->
                 let todo t = PromoteTodo
                                { promTile = t
                                , promStartFrom = aid1
                                , promExclude = []
                                }
                 in autoPromote g1 (map todo proms)
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
    CompleteMarket {} -> Failed "A market still needs to be completed."

rejoin :: AreaId -> Tile -> MarketId -> Game -> Perhaps Game
rejoin aid t mid g =
  do whenReady g
     let ot = OwnedTile { tileType = t, tileOwner = gameCurPlayerId g }
     g1 <- updArea aid g (\a -> addTileArea mid ot =<< rmVagrant ot a)
     pure (nextTurn g1)

vagrantMoveSwap ::
  AreaId -> Tile -> GlobMarketId -> Tile -> MarketId -> Game -> Perhaps Game
vagrantMoveSwap = undefined

vagrantJumpQueue ::
  AreaId -> Tile ->
  GlobMarketId -> Game -> Perhaps Game
vagrantJumpQueue = undefined


