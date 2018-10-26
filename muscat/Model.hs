{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(unless,when,liftM,ap)


import Util.Perhaps

--------------------------------------------------------------------------------
-- Configuration

-- | How many types of tiles are there
tileNum :: Int
tileNum = 4

-- | How many of each tile do players start with
tileOfType :: Int
tileOfType = 4

-- | How many areas do we have to climb before the palace
areaNum :: Int
areaNum = 4

-- | How many tiles are promoted when a market is completed
winnerNum :: Int
winnerNum = ceiling (fromIntegral (tileNum - 1) / 2 :: Float)

-- | How many markets are in each area
marketsPerArea :: Int -> Int
marketsPerArea p
  | p <= 3    = 3
  | otherwise = p

-- | How many tiles do we need in the palace to end the game
promotedEnds :: Int -> Int
promotedEnds p = 4 + 2 * div p 2
--------------------------------------------------------------------------------



newtype Updater s a = U { unU :: s -> Perhaps (a,s) }

instance Functor (Updater s) where
  fmap = liftM

instance Applicative (Updater s) where
  pure a = U (\s -> Ok (a,s))
  (<*>)  = ap

instance Monad (Updater s) where
  U m >>= k = U (\s -> do (a,s1) <- m s
                          unU (k a) s1)

updater :: s -> Updater s a -> Perhaps (a,s)
updater s (U m) = m s

failure :: Text -> Updater s a
failure msg = U (\_ -> Failed msg)

get :: Updater s s
get = U (\s -> Ok (s,s))

set :: s -> Updater s ()
set s = U (\_ -> Ok ((),s))

upd :: (s -> s) -> Updater s ()
upd f = do s <- get
           set (f s)

ask :: (s -> a) -> Updater s a
ask f = do s <- get
           pure (f s)


data FieldOf r a = Field { getField :: r -> a
                         , setField :: a -> r -> r }

with :: FieldOf r f -> Updater f a -> Updater r a
with f m =
  do s <- get
     case unU m (getField f s) of
       Ok (a,si)  -> do set (setField f si s)
                        pure a
       Failed err -> failure err







--------------------------------------------------------------------------------
-- Tiles

newtype Tile      = Tile Int
                    deriving (Eq,Ord,Show)

baseTiles :: [Tile]
baseTiles = [ Tile n | n <- take tileNum [ 0 .. ] ]

nextTile :: Tile -> Tile
nextTile (Tile x) = Tile (mod (x + 1) tileNum)

data OwnedTile    = OwnedTile { tileType :: Tile, tileOwner :: PlayerId }
                    deriving Eq
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

marketIsFull :: Market -> Bool
marketIsFull m = Set.size (marketMissing m) == 1

marketAccepts :: Tile -> Market -> Bool
marketAccepts t m = not (marketIsFull m) && (t `Set.member` marketMissing m)


addTileMarket :: OwnedTile -> Updater Market ()
addTileMarket ot =
  do full <- ask marketIsFull
     when full (failure "Market is full.")

     let t = tileType ot

     has <- ask marketHas
     when (t `Map.member` has)
       $ failure "Market already has this tile."

     upd $ \m ->
        Market { marketHas     = Map.insert t (tileOwner ot) (marketHas m)
               , marketMissing = Set.delete t (marketMissing m)
               }

rmTileMarket :: OwnedTile -> Updater Market ()
rmTileMarket OwnedTile {..} =
  do Market {..} <- get
     case Map.lookup tileType marketHas of
       Just pid | pid == tileOwner ->
          do set Market { marketHas = Map.delete tileType marketHas
                        , marketMissing = Set.insert tileType marketMissing }
       _ -> failure "Market does not have this tile."


completeMarket :: Updater Market ([OwnedTile],[OwnedTile])
completeMarket =
  do Market{..} <- get
     case Set.minView marketMissing of
       Just (a,bs)
         | Set.null bs ->
           do set emptyMarket
              pure $ splitAt winnerNum
                   $ map toOwnedTile
                   $ Map.toList sooner ++ Map.toList later
         where
         (later,sooner)    = Map.split a marketHas
         toOwnedTile (t,p) = OwnedTile { tileType = t, tileOwner = p }

       _ -> failure "Market is not yet full."


vagrantJumpMarket :: OwnedTile -> Updater Market ()
vagrantJumpMarket ot =
  do full       <- ask marketIsFull
     unless full (failure "This market is not full.")

     let t  = tileType ot
         t1 = nextTile t
         t2 = nextTile t1
         owner = tileOwner ot

     Market{..} <- get
     unless (t1 `Set.member` marketMissing)
        (failure "Vagrant can jump only to the front of the line.")

     case Map.lookup t2 marketHas of
       Just pid | owner == pid ->
          set Market { marketHas     = Map.insert t1 owner marketHas
                     , marketMissing = Set.insert t2 marketMissing
                     }
       _ -> failure "Player needs to own last place."


marketMoveSwap :: OwnedTile -> Updater Market (Maybe PlayerId)
marketMoveSwap ot =
  do full <- ask marketIsFull
     if full then doSwap else doMove

  where
  t = tileType ot

  doSwap =
    do has <- ask marketHas
       case Map.lookup t has of
         Nothing  -> failure "This market is already full."
         Just pid ->
           do upd $ \m -> m { marketHas = Map.insert t (tileOwner ot) has }
              pure (Just pid)

  doMove =
    do addTileMarket ot
       full <- ask marketIsFull
       unless full (failure "Market must be full after move.")
       pure Nothing



--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Areas

data Area         = Area { areaMarkets :: Map MarketId Market
                         , areaStreet  :: [OwnedTile]
                         }

newtype MarketId  = MarketId Int deriving (Eq,Ord)

market :: MarketId -> FieldOf Area Market
market mid = Field { .. }
  where
  getField a   = Map.findWithDefault emptyMarket mid (areaMarkets a)
  setField m a = a { areaMarkets = Map.insert mid m (areaMarkets a) }


emptyArea :: Int -> Area
emptyArea n = Area { areaMarkets = markets
                   , areaStreet  = [] }
  where
  markets = Map.fromList [ (MarketId i,emptyMarket) | i <- take n [ 0 .. ] ]



addTileArea :: MarketId -> OwnedTile -> Updater Area ()
addTileArea mid ot = with (market mid) (addTileMarket ot)


completeMarketArea :: MarketId -> Updater Area [OwnedTile]
completeMarketArea mid =
  do (win,loose) <- with (market mid) completeMarket
     upd $ \a -> a { areaStreet = loose ++ areaStreet a }
     pure win


-- XXX: non-determinism
autoPromoteArea :: [MarketId] -> OwnedTile -> Area -> [(MarketId,Area)]
autoPromoteArea excluded ot a =
  [ (mid,a1) | mid   <- Map.keys (areaMarkets a)
             , not (mid `elem` excluded)
             , Ok (_,a1) <- [ updater a (addTileArea mid ot) ]
             ]

rmVagrantArea :: OwnedTile -> Updater Area ()
rmVagrantArea ot =
  do street <- ask areaStreet
     case break (ot ==) street of
       (xs,_:ys) -> upd $ \a -> a { areaStreet = xs ++ ys }
       _ -> failure "Area does not have the required vagrant."


areaMoveSwap :: OwnedTile -> MarketId -> MarketId -> Updater Area Bool
areaMoveSwap ot from to =
  do mb <- with (market to) (marketMoveSwap ot)
     with (market from) $
       do rmTileMarket ot
          case mb of
            Nothing  -> pure ()
            Just pid -> addTileMarket ot { tileOwner = pid }
          ask marketIsFull

vagrantCanRejoin :: PlayerId -> Area -> Bool
vagrantCanRejoin pid a =
  any canRejoin [ tileType ot | ot <- areaStreet a, tileOwner ot == pid ]
  where
  canRejoin t = any (marketAccepts t) (areaMarkets a)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Turn order

data PlayerOrder  = PlayerOrder { playersDone :: [PlayerId]
                                , curPlayer   :: PlayerId
                                , nextPlayers :: [PlayerId]
                                , firstPlayer :: PlayerId
                                }

numberOfPlayers :: PlayerOrder -> Int
numberOfPlayers p = 1 + length (playersDone p) + length (nextPlayers p)

atRoundStart :: PlayerOrder -> Bool
atRoundStart o = curPlayer o == firstPlayer o

advanceTurnOrder :: Updater PlayerOrder ()
advanceTurnOrder =
  do PlayerOrder { .. } <- get
     case nextPlayers of
       x : xs -> set PlayerOrder
                       { playersDone = curPlayer : playersDone
                       , curPlayer = x
                       , nextPlayers = xs
                       , .. }
       [] -> case reverse (curPlayer : playersDone) of
               ~(x : xs) -> set PlayerOrder { playersDone = []
                                            , curPlayer = x
                                            , nextPlayers = xs
                                            , .. }

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Promoting things

-- | Where a promoted tile can go.
data PromotionTarget = PromotionTarget
  { promoteArea     :: AreaId             -- ^ Going to this area
  , promoteMarkets  :: Map MarketId Area  -- ^ One of these markets
  }

-- | Identifies any market in the game.
data GlobMarketId = GlobMarketId { gmAID :: AreaId, gmMID :: MarketId }
                      deriving (Eq,Ord)

-- | These need to be promoted.
data PromoteTodo = PromoteTodo
  { promExclude   :: [GlobMarketId] -- ^ Don't go there, as others already went
  , promStartFrom :: AreaId         -- ^ First area to try
  , promTile      :: OwnedTile      -- ^ This is the tile thwith needs promotion
  }

-- | Which of the excluded markets are on the current area
promExcludeStart :: PromoteTodo -> [ MarketId ]
promExcludeStart p = [ gmMID gmid | gmid <- promExclude p
                                  , gmAID gmid /= promStartFrom p ]



--------------------------------------------------------------------------------
-- Players

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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Game

data Game         = Game { gameAreas    :: Map AreaId Area
                         , gamePlayers  :: Map PlayerId Player
                         , gamePalace   :: [OwnedTile]
                         , gameOrder    :: PlayerOrder
                         , gameRemoved  :: [OwnedTile]
                         , gameStatus   :: GameStatus
                         }


newtype PlayerId  = PlayerId Int deriving (Eq,Ord)
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

--------------------------------------------------------------------------------
-- Complex fields of Game

area :: AreaId -> FieldOf Game Area
area aid = Field { .. }
  where
  getField g   = case Map.lookup aid (gameAreas g) of
                   Just a  -> a
                   Nothing -> emptyArea (numberOfPlayers (gameOrder g))
  setField a g = g { gameAreas = Map.insert aid a (gameAreas g) }

player :: PlayerId -> FieldOf Game Player
player pid = Field { .. }
  where
  getField g = case Map.lookup pid (gamePlayers g) of
                 Just p  -> p
                 Nothing -> error "Invalid player id"
  setField p g = g { gamePlayers = Map.insert pid p (gamePlayers g) }

turnOrder :: FieldOf Game PlayerOrder
turnOrder = Field { .. }
  where
  getField = gameOrder
  setField t g = g { gameOrder = t }
--------------------------------------------------------------------------------


gameCurPlayerId :: Game -> PlayerId
gameCurPlayerId = curPlayer . gameOrder

addPalace :: OwnedTile -> Updater Game ()
addPalace ot = upd $ \g -> g { gamePalace = ot : gamePalace g }

whenReady :: Updater Game ()
whenReady =
  do status <- ask gameStatus
     case status of
       NextTurn     -> pure ()
       GameFinished -> failure "This game has finished."
       Promote {}   -> failure "Some tiles still need to be promoted."
       CompleteMarket {} ->
        failure "You need to choose which market to promote."

autoPromote :: [PromoteTodo] -> Updater Game ()
autoPromote ots =
  case ots of
    [] -> nextTurn
    todo : rest ->
      do let aid = promStartFrom todo
             ot  = promTile todo

         opts <- with (area aid)
                 $ ask $ autoPromoteArea (promExcludeStart todo) ot

         case opts of
           [] ->
             case nextArea aid of
               Nothing -> do addPalace ot
                             autoPromote rest
               Just aid1 -> autoPromote (todo { promStartFrom = aid1 }:rest)

           [(mid,a1)] ->
             do with (area aid) $ set a1
                let gmid = GlobMarketId { gmAID = aid, gmMID = mid }
                    avoid t = t { promExclude = gmid : promExclude t }
                autoPromote (map avoid rest)

           _ -> do let tgt = PromotionTarget
                               { promoteArea    = aid
                               , promoteMarkets = Map.fromList opts }
                   upd $ \g -> g { gameStatus = Promote tgt rest }


nextTurn :: Updater Game ()
nextTurn =
  do atStart <- with turnOrder $ do advanceTurnOrder
                                    start <- ask firstPlayer
                                    cur   <- ask curPlayer
                                    pure (start == cur)
     when atStart checkGameFinished


addTile :: Updater Player Tile -> MarketId -> Updater Game ()
addTile chooseTile mid =
  do whenReady
     cur <- with turnOrder (ask curPlayer)
     t   <- with (player cur) chooseTile
     let ot = OwnedTile { tileType = t, tileOwner = cur }
     with (area firstArea) (addTileArea mid ot)
     nextTurn


checkGameFinished :: Updater Game ()
checkGameFinished =
  do promNum <- ask (length . gamePalace)
     pnum    <- with turnOrder (ask numberOfPlayers)
     when (promNum >= promotedEnds pnum) $
       upd $ \g -> g { gameStatus = GameFinished }

owned :: Tile -> Game -> OwnedTile
owned t g = OwnedTile { tileType = t, tileOwner = gameCurPlayerId g }

--------------------------------------------------------------------------------
-- Entry Points

addVisTile :: MarketId -> Updater Game ()
addVisTile = addTile useVisible

addBlindTile :: MarketId -> Updater Game ()
addBlindTile = addTile useBlind

complete :: AreaId -> MarketId -> Updater Game ()
complete aid mid =
  do status <- ask gameStatus
     case status of
       NextTurn -> doComplete
       CompleteMarket aid1 mid1 mid2
         | aid == aid1 && (mid == mid1 || mid == mid2) -> doComplete
         | otherwise -> failure "This is not a valid market to complete."

       GameFinished -> failure "This game has finished."
       Promote {} -> failure "Some tiles still need to be promoted"

  where
  doComplete =
    do proms <- with (area aid) (completeMarketArea mid)
       case nextArea aid of
         Nothing ->
           do upd $ \g -> g { gamePalace = proms ++ gamePalace g }
              nextTurn
         Just aid1 ->
            do let todo t = PromoteTodo
                             { promTile = t
                             , promStartFrom = aid1
                             , promExclude = []
                             }
               autoPromote (map todo proms)

promote :: MarketId -> Updater Game ()
promote mid =
  do status <- ask gameStatus
     case status of
       Promote pt rest ->
         case Map.lookup mid (promoteMarkets pt) of
           Just a ->
             do with (area (promoteArea pt)) $ set a
                autoPromote rest
           Nothing -> failure "That's not a valid promotion choice."
       GameFinished      -> failure "This game has finished."
       NextTurn          -> failure "There is nothing to promote."
       CompleteMarket {} -> failure "A market still needs to be completed."

rejoin :: AreaId -> Tile -> MarketId -> Updater Game ()
rejoin aid t mid =
  do whenReady
     ot <- ask (owned t)
     with (area aid) $ do rmVagrantArea ot
                          addTileArea mid ot
     nextTurn


vagrantMoveSwap ::
  AreaId -> Tile -> GlobMarketId -> Tile -> MarketId -> Updater Game ()
vagrantMoveSwap vaid vt from t toMID =
  do whenReady
     -- spend vagrant
     vtile <- ask (owned vt)
     with (area vaid) (rmVagrantArea vtile)

     -- move or swap tile
     let aid     = gmAID from
         fromMID = gmMID from
     tile <- ask (owned t)
     both <- with (area aid) (areaMoveSwap tile fromMID toMID)
     if both
        then upd $ \g -> g { gameStatus = CompleteMarket aid fromMID toMID }
        else complete aid toMID

vagrantJumpQueue :: AreaId -> Tile -> GlobMarketId -> Updater Game ()
vagrantJumpQueue aid t gmid =
  do whenReady
     ot <- ask (owned t)
     with (area aid) (rmVagrantArea ot)

     let tgt = gmMID gmid
     with (area (gmAID gmid)) $
       with (market tgt) (vagrantJumpMarket ot)
     complete aid tgt

endGame :: Updater Game ()
endGame =
  do curPID  <- with turnOrder (ask curPlayer)
     noTiles <- with (player curPID) $ ask (null . playerStack)
     unless noTiles $ failure "You can't end the game if you still have tiles."
     mayRejoin <- ask (any (vagrantCanRejoin curPID) . gameAreas)
     when mayRejoin $ failure "You still have vagrants who can rejoin."
     upd $ \g -> g { gameStatus = GameFinished }




