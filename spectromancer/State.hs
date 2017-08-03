{-# Language OverloadedStrings, RecordWildCards #-}
module State where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS

import Util.Random(Gen,StdGen, genRandFun, randSourceIO)

import CardTypes
import Deck

data Player = Player
  { playerLife    :: Int
  , playerDeck    :: Deck
  , playerPower   :: Map Element Int
  , playerActive  :: Map Slot Card
  , playerName    :: Text
  } deriving Show

newPlayer :: Text -> Deck -> Gen Player
newPlayer playerName playerDeck =
  do let playerPower = Map.fromList [ (e,3) | e <- allElements ]
     return Player { playerLife = startLife, playerActive = Map.empty, .. }

type Slot = Int


data Game = Game
  { curPlayer   :: Player
  , otherPlayer :: Player
  , gameRNG     :: StdGen
  } deriving Show


slotNum :: Int
slotNum = 6

startLife :: Int
startLife = 60

newGame :: StdGen -> (Text,Class) -> (Text,Class) -> Game
newGame rng (name1,class1) (name2,class2) =
  genRandFun rng $
    do (deck1, deck2) <- pickDecks class1 class2
       curPlayer   <- newPlayer name1 deck1
       otherPlayer <- newPlayer name2 deck2
       return $ \gameRNG -> Game { .. }

playCard :: Element -> Int -> Int -> Game -> Either Text Game
playCard e n l g
  | l >= 0 && l < slotNum =
  case c of
    Nothing -> Left "Unknown card"
    Just c  ->
      Right g
        { curPlayer = otherPlayer g
        , otherPlayer = p { playerActive = Map.insert l c (playerActive p) }
        }

  | otherwise = Left "Invalid location"
  where
  p = curPlayer g
  d = playerDeck p
  c = do cs <- Map.lookup e d
         case splitAt n cs of
           (_,x:_) -> Just x
           _ -> Nothing



newGameIO :: (Text,Class) -> (Text,Class) -> IO Game
newGameIO p1 p2 =
  do gen <- randSourceIO
     return (newGame gen p1 p2)

getActiveDeck :: Game -> Map Element [Int]
getActiveDeck g = Map.fromListWith (++)
                    [ (el,[ix]) | (el,cards) <- Map.toList d
                                , (ix,c) <- zip [ 0 .. ] cards
                                , hasPower el c
                                ]
  where
  p = curPlayer g
  d = playerDeck p
  hasPower el c = case Map.lookup el (playerPower p) of
                    Nothing -> False
                    Just n  -> n >= cardCost c

--------------------------------------------------------------------------------
-- JSON Serialization

jsElementMap :: ToJSON a => Map Element a -> JS.Value
jsElementMap = JS.object . map toField . Map.toList
  where toField (e,x) = Text.pack (show e) .= x

instance ToJSON Player where
  toJSON Player { .. } = JS.object
    [ "name"    .= playerName
    , "life"    .= playerLife
    , "deck"    .= jsElementMap playerDeck
    , "power"   .= jsElementMap playerPower
    , "active"  .= [ Map.lookup s playerActive | s <- take slotNum [ 0 .. ] ]
    ]

instance ToJSON Game where
  toJSON g@Game { .. } = JS.object
    [ "current"    .= curPlayer
    , "other"      .= otherPlayer
    , "activeDeck" .= jsElementMap (getActiveDeck g)
    ]

