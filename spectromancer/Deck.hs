{-# Language OverloadedStrings #-}
module Deck (Deck, pickDecks, Element(..), allElements, Class) where

import Data.List(find)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)

import Util.Random(Gen,shuffle,randSourceIO,genRand)

import CardTypes(Card(..))
import Cards(allCards)

import Text.Show.Pretty

test =
  do g <- randSourceIO
     let ((d1,d2),_) = genRand g (pickDecks "VAMPIRIC CARDS" "SORCERY CARDS")
     pPrint $ fmap (map cardName) d1
     putStrLn "========================="
     pPrint $ fmap (map cardName) d2

type Deck = Map Element [ Card ]
type Class = Text

data Element = Fire | Water | Air | Earth | Special
                deriving (Show,Eq,Ord,Enum,Bounded)

allElements :: [Element]
allElements = [minBound .. maxBound]


pickDecks :: Class -> Class -> Gen (Deck,Deck)
pickDecks special1 special2 =
  do (f1,f2) <- pickCategory "FIRE CARDS"
     (a1,a2) <- pickCategory "AIR CARDS"
     (e1,e2) <- pickCategory "EARTH CARDS"
     (w1,w2) <- pickCategory "WATER CARDS"
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
validDeck ds = and [ ban "Orc Chieftain" "Forest Sprite"
                   , ban "Meditation"    "Stone Rain"
                   , ban "Inferno"       "Armageddon"
                   , ban "Elf Hermit"    "Nature's Fury"
                   , if has "Phoenix"
                       then atMostOne [ "Armageddon"
                                      , "Acidic Rain"
                                      , "Stone Rain"
                                      , "Drain Souls" ]
                        else True
                   , ban "Greater Demon" "Armageddon"
                   , ban "Armageddon" "Wall of Reflection"
                   , ban "Nature's Ritual" " Wall of Reflection"
                   , ban "Meditation" "Cursed Fog"
                   , ban "Ice Golem" "Cursed Fog"
                   , ban "Nature's Ritual" "Chrono Engine"
                   , ban "Ice Golem" "army of Rats"
                   , ban "Elf Hermit" "Rescue Operation"
                   , ban "Orc Chieftain" "Forest Wolf"
                   , ban "Orc Chieftain" "Devoted Servant"
                   , ban "Sea Sprite" "Chastiser"
                   , ban "Giant Spider" "Vampire Mystic"
                   , ban "Ice Golem" "Greater Bargul"
                   , ban "Nature's Fury" "Greater Bargul"
                   , ban "Astral Guard" "Reaver"
                   , ban "Ice Golem" "Stone Rain"
                   , ban "Ice Golem" "Armageddon"
                   ]
  where
  has x = case find ((x ==) . cardName) ds of
            Just _ -> True
            _      -> False

  ban x y = if has x then not (has y) else True

  atMostOne xs = case xs of
                   []     -> True
                   x : ys -> if has x then all (not . has) ys else atMostOne ys

