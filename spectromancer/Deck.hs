{-# Language OverloadedStrings #-}
module Deck where

import Data.List(find)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)

import Util.Random(Gen,shuffle)

import CardTypes(Card(..))
import Cards(allCards)

type Deck = Map Element [ Card ]

data Element = Fire | Water | Air | Earth | Special
                deriving (Show,Eq,Ord)


pickCategory :: Text -> Gen ([Card],[Card])
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

