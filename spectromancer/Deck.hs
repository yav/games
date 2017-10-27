{-# Language OverloadedStrings #-}
module Deck (Deck, pickDecks, Element(..), allElements, Class, genInitialMana) where

import Data.List(find)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Data.Aeson(ToJSON(..))
import Control.Lens(view)
import Control.Monad(replicateM)

import Util.Random(Gen,shuffle,randInRange)

import CardTypes(cardName,Card)
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

-- | The first list tells us how to chunk the second list.
splitInto :: Int -> [Int] -> [Int] -> [[Int]]
splitInto n (x : xs) ys =
  let end = n + x
  in case span (< end) ys of
       (as, bs) -> as : splitInto end xs bs
splitInto _ [] _ = []

-- | Insert in a sorted list.
insert :: Int -> [Int] -> [Int]
insert x xs =
  case xs of
    [] -> [x]
    y : more
      | x < y     -> x : y : more
      | otherwise -> y : insert x more

-- | Pick indexes of cards
pickCategory :: Element -> Gen ([Int],[Int])
pickCategory el =
  do let ixes = take cardNum [ 0 .. ]
     picked <- mapM fromChunk (splitInto 0 chunkNum ixes)
     let (xs,ys) = unzip picked
         used   = xs ++ ys
         keep c = not (c `elem` used)
     case el of
       Special -> return (xs, ys)
       _       -> do (x,y) <- fromChunk (filter keep ixes)
                     return (insert x xs, insert y ys)
  where
  cardNum = sum chunkNum
  chunkNum = case el of
               Fire    -> [ 4, 5, 3 ]
               Water   -> [ 3, 5, 4 ]
               Air     -> [ 3, 5, 4 ]
               Earth   -> [ 4, 5, 3 ]
               Special -> [ 2, 2, 2, 2 ]




pickDecks :: Class -> Class -> Gen (Deck,Deck)
pickDecks special1 special2 =
  do let toC xs c           = map ((allCards Map.! c) !!) xs
         toC2 c1 c2 (xs,ys) = (toC xs c1, toC ys c2)

     (f1,f2) <- toC2 fire_cards  fire_cards  <$> pickCategory Fire
     (a1,a2) <- toC2 air_cards   air_cards   <$> pickCategory Air
     (e1,e2) <- toC2 earth_cards earth_cards <$> pickCategory Earth
     (w1,w2) <- toC2 water_cards water_cards <$> pickCategory Water
     (s1,s2) <- toC2 special1   special2     <$> pickCategory Special

     let deck f a e w s = Map.fromList [ (Fire,f), (Water,w), (Air,a)
                                       , (Earth,e), (Special,s) ]

     if validDeck special1 (concat [f1,a1,e1,w1,s1]) &&
        validDeck special2 (concat [f2,a2,e2,w2,s2])
        then return (deck f1 a1 e1 w1 s1, deck f2 a2 e2 w2 s2)
        else pickDecks special1 special2



fromChunk :: [a] -> Gen (a,a)
fromChunk xs =
  do a : b : _ <- shuffle xs
     return (a,b)


validDeck :: Class -> [Card] -> Bool
validDeck c ds =
               and [ numRange (1,1) [ fire_priest_of_fire
                                    , water_merfolk_elder
                                    , earth_elf_hermit ]
                   , numRange (1,1) [ fire_flame_wave
                                    , fire_inferno
                                    , air_chain_lightning ]
                   , numRange (1,2) [ air_chain_lightning
                                    , fire_armageddon
                                    , air_lightning_bolt
                                    , earth_natures_fury ]
                   , numRange (2,4) [ water_ice_guard
                                    , water_water_elemental
                                    , air_faerie_sage
                                    , earth_elven_healer
                                    , earth_natures_ritual
                                    , earth_rejuvenation
                                    , earth_master_healer ]

                   , ban fire_orc_chieftain earth_forest_sprite
                   , ban water_meditation   earth_stone_rain
                   , ban fire_inferno       fire_armageddon
                   , ban earth_elf_hermit   earth_natures_fury

                   , if has air_phoenix
                       then numRange (0,1) [ fire_armageddon
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
                   , onlyIf vampiric_cards $
                        ban fire_orc_chieftain earth_giant_spider
                   , ban water_sea_sprite vampiric_chastiser
                   , ban earth_giant_spider vampiric_vampire_mystic
                   , ban water_ice_golem cult_greater_bargul
                   , ban earth_natures_fury cult_greater_bargul
                   , ban water_astral_guard cult_reaver
                   , onlyIf golem_cards $ ban water_ice_golem earth_stone_rain
                   , onlyIf golem_cards $ ban water_ice_golem fire_armageddon
                   , onlyIf golem_cards $ ban fire_armageddon earth_stone_rain
                   ]
  where

  onlyIf x p = if c == x then p else True

  has = hasCard ds

  ban x y = if has x then not (has y) else True

  numRange (least,most) xs = let have = length (filter has xs)
                             in least <= have && have <= most


hasCard :: [Card] -> Text -> Bool
hasCard ds x = case find ((x ==) . view cardName) ds of
      Just _ -> True
      _      -> False

bucketize :: Int -> Int -> (Int, Int) -> Gen [Int]
bucketize n s (mn, mx) =
  do let lb = s - mn * n
     rands <- replicateM (n - 1) (randInRange mn mx)
     let r =  (s - sum rands)
         valid x = mn <= x && x <= mx

     if valid r then return (r : rands) else bucketize n s (mn, mx)

randBuckets :: Int -> Int -> (Int, Int) -> Gen [Int]
randBuckets n s (mn, mx) =
  do b <- bucketize n s (mn, mx)
     s <- shuffle b
     return s

initialMana :: Int -> Gen (Map Element Int)
initialMana s =
  do pl <- randBuckets 4 s (3, 6)
     return $
        Map.fromList $ (Special, 2) : zip [Fire, Earth, Air, Water] pl

validMana :: Deck -> (Map Element Int) -> Bool
validMana d e =
  and
  [  not (power Fire < 3 && has fire_priest_of_fire)
  ,  not (power Water < 5 && has water_merfolk_elder)
  ,  not (power Earth < 5 && has earth_elf_hermit)
  ,  not (power Water > 4 && has sorcery_sacrifice
      && (has water_mind_master || has water_astral_guard)) ]

  where has c = any (\cs -> hasCard cs c) (Map.elems d)
        power elt = e Map.! elt

genInitialMana :: Bool -> Deck -> Gen (Map Element Int)
genInitialMana isFirst deck =
  do m <- initialMana amt
     if validMana deck m then return m else genInitialMana isFirst deck
  where amt = if isFirst then 19 else 18
