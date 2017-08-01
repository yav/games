{-# Language OverloadedStrings, RecordWildCards #-}
module CardTypes where

import           Data.Text(Text)
import           Data.Map(Map)
import qualified Data.Map as Map
import           Text.Read(readMaybe)
import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON(..),FromJSON(..), Object, (.:), (.=))
import           Data.Aeson.Types(Parser)
import qualified Data.HashMap.Lazy as HashMap
import           Control.Monad(guard,forM)
import qualified Data.ByteString.Lazy as LBS
import           Text.Show.Pretty(ppShow)

type Category = Text
type Cards    = Map Category [Card]

data Card = Card
  { cardName        :: Text
  , cardDescription :: Text
  , cardImage       :: FilePath
  , cardCost        :: Int
  , cardEffect      :: CardEffect
  } deriving Show

data CardEffect = Spell | Creature CreatureCard
  deriving Show

data CreatureCard = CreatureCard
  { creatureAttack      :: Maybe Int
  , creatureLife        :: Int
  } deriving Show

instance ToJSON Card where
  toJSON Card { .. } = JS.object fields
    where
    fields = special ++ common
    common = [ "name" .= cardName
             , "description" .= cardDescription
             , "image" .= cardImage
             , "cost" .= cardCost
             ]
    special = case cardEffect of
                Spell -> [ "type" .= ("spell" :: Text) ]
                Creature CreatureCard { .. } ->
                  [ "type" .= ("creature" :: Text)
                  , "attack" .= creatureAttack
                  , "life"   .=  creatureLife
                  ]


instance FromJSON Card where
  parseJSON = JS.withObject "card object" $ \o ->
                do cardName        <- o .: "name"
                   cardDescription <- o .: "description"
                   cardImage       <- o .: "image"
                   cardCost        <- parseNumField o "cost"
                   tag             <- o .: "type"
                   cardEffect <- case tag :: Text of
                                   "creature" -> do c <- parseCreature o
                                                    return (Creature c)
                                   "spell"    -> return Spell
                                   _ -> fail ("Unknown card tag: " ++ show tag)
                   return Card { .. }

parseFromFile :: FilePath -> IO ()
parseFromFile file =
  do bytes <- LBS.readFile file
     case JS.eitherDecode bytes of
       Left err -> fail err
       Right cs -> writeFile "Cards.hs" (ppShow (cs :: Cards))


cardsToJSON :: Cards -> JS.Value
cardsToJSON cs = JS.object [ k .= v | (k,v) <- Map.toList cs ]

parseCards :: JS.Value -> Parser Cards
parseCards = JS.withObject "categories" $ \o ->
  do parsedPairs <- forM (HashMap.toList o) $ \(cat,v) ->
                      do cs <- parseJSON v
                         return (cat, cs)
     return (Map.fromList parsedPairs)

parseNumField :: Object -> Text -> Parser Int
parseNumField o f =
  do txt <- o .: f
     case readMaybe txt of
       Just r  -> return r
       Nothing -> fail ("Failed to parse nuber in field: " ++ show f)

parseCreature :: Object -> Parser CreatureCard
parseCreature o =
  do creatureLife        <- parseNumField o "life"
     creatureAttackTxt   <- o .: "attack"
     let creatureAttack = do r <- readMaybe creatureAttackTxt
                             guard (r >= 0)
                             return r
     return CreatureCard { .. }



