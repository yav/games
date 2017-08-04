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
  , playerDeck    :: Map Element [DeckCard]
  , playerPower   :: Map Element Int
  , playerActive  :: Map Slot DeckCard
  , playerName    :: Text
  } deriving Show

-- | A card in a player's deck.
data DeckCard = DeckCard
  { deckCardOrig      :: Card      -- ^ Unmodified deck card
  , deckCardElement   :: Element   -- ^ Card's element
  , deckCard          :: Card      -- ^ Current version of the card
  , deckCardEnabled   :: Bool      -- ^ Is it currently playable
  } deriving Show


inactiveCard :: Element -> Card -> DeckCard
inactiveCard deckCardElement deckCardOrig =
  DeckCard { deckCard = deckCardOrig
           , deckCardEnabled = False
           , ..
           }

newPlayer :: Text -> Deck -> Gen Player
newPlayer playerName deck =
  do let playerPower = Map.fromList [ (e,3) | e <- allElements ]
         dc e cs = map (inactiveCard e) cs
     return Player { playerLife = startLife
                   , playerActive = Map.empty
                   , playerDeck = Map.mapWithKey dc deck
                   , .. }

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
       return $ \gameRNG -> activateCards Game { .. }

playCard :: Element -> Int -> Maybe Location -> Game -> Either Text Game
playCard e n mbL g =
  case mbC of
    Nothing -> Left "Unknown card"
    Just c
      | not (targetOk (cardTarget (deckCard c))) -> Left "Invalid target"
      | otherwise ->
        Right $ activateCards
              $ startTurn
              $ switchPlayers
              $ (if isSpell (deckCard c)
                   then g
                   else case mbL of
                          Just l ->
                           g { curPlayer =
                             p { playerActive =
                                  Map.insert (locWhich l) c (playerActive p) } }
                          _ -> g)

  where
  p = curPlayer g
  d = playerDeck p
  mbC = do cs <- Map.lookup e d
           case splitAt n cs of
             (_,x:_) -> Just x
             _ -> Nothing

  otherActive = playerActive (otherPlayer g)

  targetOk tgt =
    case mbL of
      Nothing ->
        case tgt of
          NoTarget -> True
          _        -> False
      Just Location { .. } ->
        case tgt of

          NoTarget -> False

          TargetCasterBlank ->
            locWho == Caster &&
            0 >= locWhich &&
            locWhich < slotNum &&
            not (n `Map.member` playerActive p)

          TargetCaster's ->
            locWho == Caster && locWhich `Map.member` playerActive p

          TargetOpponent's ->
            locWho == Opponent && locWhich `Map.member` otherActive

          TargetOpponent'sNormal ->
            locWho == Opponent &&
            (case Map.lookup n otherActive of
               Just x -> deckCardElement x /= Special
               _      -> False)

          TargetCreature ->
            case locWho of
              Caster   -> locWhich `Map.member` playerActive p
              Opponent -> locWhich `Map.member` otherActive



newGameIO :: (Text,Class) -> (Text,Class) -> IO Game
newGameIO p1 p2 =
  do gen <- randSourceIO
     return (newGame gen p1 p2)


switchPlayers :: Game -> Game
switchPlayers Game { .. } = Game { curPlayer = otherPlayer
                                 , otherPlayer = curPlayer
                                 , .. }


-- | Compute which cards in the deck are playable this turn and
-- disable opponents cards
activateCards :: Game -> Game
activateCards g =
  g { curPlayer   = c { playerDeck = Map.mapWithKey activate (playerDeck c) }
    , otherPlayer = o { playerDeck = Map.map      deactivate (playerDeck o) }
    }
  where
  c = curPlayer g
  o = otherPlayer g

  activate el cards = [ DeckCard { deckCardEnabled = active deckCard, .. }
                                              | DeckCard { .. } <- cards ]
    where
    have = Map.findWithDefault 0 el (playerPower c)
    active card = cardCost card <= have && hasTarget (cardTarget card)
    hasTarget tgt =
      case tgt of
        NoTarget -> True
        TargetCasterBlank ->
          Map.size (playerActive c) < slotNum
        TargetCaster's ->
          not (Map.null (playerActive c))
        TargetOpponent's ->
          not (Map.null (playerActive o))
        TargetOpponent'sNormal ->
          any ((/= Special) . deckCardElement) (Map.elems (playerActive o))
        TargetCreature ->
          not (Map.null (playerActive c) && Map.null (playerActive o))



  deactivate cards = [ d { deckCardEnabled = False } | d <- cards ]

startTurn :: Game -> Game
startTurn g = g { curPlayer = p1 }
  where
  p = curPlayer g
  p1 = p { playerPower = fmap (+1) (playerPower p) }



--------------------------------------------------------------------------------
-- JSON Serialization

jsElementMap :: ToJSON a => Map Element a -> JS.Value
jsElementMap = JS.object . map toField . Map.toList
  where toField (e,x) = Text.pack (show e) .= x

instance ToJSON DeckCard where
  toJSON DeckCard { .. } =
    JS.object [ "card"    .= deckCard -- XXX: add stats from original in desc.
              , "enabled" .= deckCardEnabled
              , "element" .= deckCardElement
              , "target"  .= cardTarget deckCard
              ]

instance ToJSON Player where
  toJSON Player { .. } = JS.object
    [ "name"    .= playerName
    , "life"    .= playerLife
    , "deck"    .= jsElementMap playerDeck
    , "power"   .= jsElementMap playerPower
    , "active"  .= [ Map.lookup s playerActive | s <- take slotNum [ 0 .. ] ]
    ]

instance ToJSON Game where
  toJSON Game { .. } = JS.object
    [ "current"    .= curPlayer
    , "other"      .= otherPlayer
    ]

