{-# Language OverloadedStrings #-}
module Deck (Deck, pickDecks, Element(..), allElements, Class) where

import Data.List(find)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Data.Aeson(ToJSON(..))

import Util.Random(Gen,shuffle)

import CardTypes(Card(..))
import Cards(allCards)
import CardIds

type Deck = Map Element [ Card ]
type Class = Text

data Element = Fire | Water | Air | Earth | Special
                deriving (Show,Eq,Ord,Enum,Bounded)

instance ToJSON Element where
  toJSON el = toJSON (show el)

allElements :: [Element]
allElements = [minBound .. maxBound]


pickDecks :: Class -> Class -> Gen (Deck,Deck)
pickDecks special1 special2 =
  do (f1,f2) <- pickCategory fire_cards
     (a1,a2) <- pickCategory air_cards
     (e1,e2) <- pickCategory earth_cards
     (w1,w2) <- pickCategory water_cards
     (s1,s2) <- if special1 == special2
                   then pickCategory special1
                   else -- XXX: In this case can we pick them separately
                        -- or are the indexes in the two specialty decks
                        -- related?
                        do (s1,_) <- pickCategory special1
                           (_,s2) <- pickCategory special2
                           return (s1,s2)
     let deck f a e w s = Map.fromList [ (Fire,f), (Water,w), (Air,a)
                                       , (Earth,e), (Special,s) ]

     if validDeck (concat [f1,a1,e1,w1,s1]) &&
        validDeck (concat [f2,a2,e2,w2,s2])
        then return (deck f1 a1 e1 w1 s1, deck f2 a2 e2 w2 s2)
        else pickDecks special1 special2

pickCategory :: Class -> Gen ([Card],[Card])
pickCategory c =
  case Map.lookup c allCards of
    Just xs -> pickFrom xs
    Nothing -> error ("Unknown category: " ++ show c)


chunks :: Int -> [a] -> [[a]]
chunks n xs =
  case xs of
    [] -> []
    _  -> let (as,bs) = splitAt n xs
          in as : chunks n bs


fromChunk :: [a] -> Gen (a,a)
fromChunk xs =
  do a : b : _ <- shuffle xs
     return (a,b)

pickFrom :: [a] -> Gen ([a]{-4-},[a])
pickFrom grp = fmap unzip (mapM fromChunk (chunks sz grp))
  where sz = length grp `div` 4


validDeck :: [Card] -> Bool
validDeck ds = and [ ban fire_orc_chieftain earth_forest_sprite
                   , ban water_meditation    earth_stone_rain
                   , ban fire_inferno       fire_armageddon
                   , ban earth_elf_hermit    earth_natures_fury
                   , if has air_phoenix
                       then atMostOne [ fire_armageddon
                                      , water_acidic_rain
                                      , earth_stone_rain
                                      , death_drain_souls ]
                        else True
                   , ban demonic_greater_demon fire_armageddon
                   , ban fire_armageddon illusion_wall_of_reflection
                   , ban earth_natures_ritual illusion_wall_of_reflection
                   , ban water_meditation death_cursed_fog
                   , ban water_ice_golem death_cursed_fog
                   , ban earth_natures_ritual time_chrono_engine
                   , ban water_ice_golem goblin's_army_of_rats
                   , ban earth_elf_hermit goblin's_rescue_operation
                   , ban fire_orc_chieftain forest_forest_wolf
                   , ban fire_orc_chieftain vampiric_devoted_servant
                   , ban water_sea_sprite vampiric_chastiser
                   , ban earth_giant_spider vampiric_vampire_mystic
                   , ban water_ice_golem cult_greater_bargul
                   , ban earth_natures_fury cult_greater_bargul
                   , ban water_astral_guard cult_reaver
                   , ban water_ice_golem earth_stone_rain
                   , ban water_ice_golem fire_armageddon
                   ]
  where
  has x = case find ((x ==) . cardName) ds of
            Just _ -> True
            _      -> False

  ban x y = if has x then not (has y) else True

  atMostOne xs = case xs of
                   []     -> True
                   x : ys -> if has x then all (not . has) ys else atMostOne ys

