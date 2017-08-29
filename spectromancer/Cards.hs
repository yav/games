{-# Language OverloadedStrings, TemplateHaskell #-}
module Cards where

import Language.Haskell.TH
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Char(isSpace,isAlpha,toLower)
import Data.Maybe(mapMaybe,listToMaybe)
import Control.Lens(view,(^.))

import qualified Data.Map as Map
import CardTypes

toIdent :: Maybe String -> Text -> Name
toIdent p = mkName . addPref . mapMaybe cvt . Text.unpack
  where
  cvt c | isAlpha c = Just (toLower c)
        | isSpace c = Just '_'
        | otherwise = Nothing
  addPref x = case p of
                Nothing -> x
                Just q  -> q ++ "_" ++ x

decl :: Maybe String -> Text -> DecsQ
decl p x = do let i = toIdent p x
              sig <- sigD i [t| Text |]
              def <- valD (varP i) (normalB (litE (stringL (Text.unpack x)))) []
              return [sig,def]

card_name_decls :: DecsQ
card_name_decls = concat <$> mapM cat (Map.toList allCards)
  where
  cat (c,cs) = do ds1 <- decl Nothing c
                  let pref = Text.unpack (Text.toLower (Text.words c !! 0))
                  dss <- mapM (decl (Just pref) . view cardName) cs
                  return (ds1 ++ concat dss)

getCard :: Text -> Text -> Card
getCard cl c = case mb of
                 Nothing   -> error ("Unknwon card: " ++ show (cl,c))
                 Just card -> card
  where mb = do xs <- Map.lookup cl allCards
                listToMaybe [ x | x <- xs, x ^. cardName == c ]


allCards :: Cards
allCards =
  Map.fromList
    [ ( "AIR CARDS"
      , [ Card
            { _cardName = "Faerie Apprentice"
            , _cardDescription =
                "Faerie Apprentice increases the damage its owner's spells deal by 1. "
            , _cardImage = "FaerieApprenticeBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 12 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Griffin"
            , _cardDescription =
                "When Griffin is summoned, if its owner's Air power is 5 or more, it deals 5 damage to the opponent. "
            , _cardImage = "GriffinBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 15 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Call to Thunder"
            , _cardDescription =
                "Call to Thunder deals 6 damage to target opponent's creature and 6 damage to the opponent. "
            , _cardImage = "CalltoThunderBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = TargetOpponent's }
        , Card
            { _cardName = "Faerie Sage"
            , _cardDescription =
                "When Faerie Sage is summoned it heals an amount of life to its owner equal to its owner's Earth power, but not more than 10. "
            , _cardImage = "FaerieSageBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 19 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Wall of Lightning"
            , _cardDescription =
                "At the beginning of its owner's turn, Wall of Lightning deals 4 damage to the opponent. "
            , _cardImage = "WallofLightningBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 0 , _creatureLife = 28 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Lightning Bolt"
            , _cardDescription =
                "Lightning Bolt deals (5+its caster's Air power) damage to the opponent. "
            , _cardImage = "LightningBoltBig.jpg"
            , _cardCost = 6
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Phoenix"
            , _cardDescription =
                "Each time Phoenix dies, if its owner's Fire power is 10 or greater, it rebirths (another Phoenix is put into the same slot). Phoenix cannot rebirth if a card says that it is \"destroyed\". "
            , _cardImage = "PhoenixBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 16 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Chain Lightning"
            , _cardDescription =
                "Chain Lightning deals 9 damage to the opponent and to each of the opponent's creatures. "
            , _cardImage = "ChainLightningBig.jpg"
            , _cardCost = 8
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Lightning Cloud"
            , _cardDescription =
                "Lightning Cloud's attack damages the opponent and each of the opponent's creatures. "
            , _cardImage = "LightningCloudBig.jpg"
            , _cardCost = 9
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 20 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Tornado"
            , _cardDescription =
                "Air spell, cost 10 Destroy target opponent's creature. "
            , _cardImage = "TornadoBig.jpg"
            , _cardCost = 10
            , _cardEffect = Spell
            , _cardTarget = TargetOpponent's }
        , Card
            { _cardName = "Air Elemental"
            , _cardDescription =
                "Attack is equal to its owner's air power. When Air Elemental is summoned it deals 8 damage to the opponent. Air Elemental increases the growth of its owner's Air power by 1. "
            , _cardImage = "AirElementalBig.jpg"
            , _cardCost = 11
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Nothing , _creatureLife = 44 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Titan"
            , _cardDescription =
                "When Titan is summoned it deals 15 damage to the creature in the opposing slot. "
            , _cardImage = "TitanBig.jpg"
            , _cardCost = 12
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 9 , _creatureLife = 40 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "BEAST CARDS"
      , [ Card
            { _cardName = "Magic Hamster"
            , _cardDescription =
                "When Magic Hamster is summoned it heals 10 life to each of its neighboring creatures. Activated ability (cost 3) Magic Hamster heals 18 life to all of its owner's creatures. "
            , _cardImage = "MagicHamsterBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 9 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Scorpion"
            , _cardDescription =
                "When Scorpion is summoned it causes the creature in the opposing slot to skip its attack next turn. Activated ability (cost 2) Scorpion deals 14 damage to target opponent's creature. "
            , _cardImage = "ScorpionBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 18 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Wolverine"
            , _cardDescription =
                "Activated ability (cost 2) Wolverine completely heals itself and its attack is increased by 2 permanently. "
            , _cardImage = "WolverineBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 25 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Energy Beast"
            , _cardDescription =
                "Activated ability (cost 1) Energy Beast increases its owner's Fire, Water, Air and Earth powers by 1. "
            , _cardImage = "EnergyBeastBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 34 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Death Falcon"
            , _cardDescription =
                "Activated ability (cost 1) Death Falcon moves to target empty slot and deals 4 damage to each of the opponent's creatures. "
            , _cardImage = "DeathFalconBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 45 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "White Elephant"
            , _cardDescription =
                "All damage that would be dealt to the owner is dealt to White Elephant instead. Activated ability (cost 0) White Elephant increases its owner's Beast power by 1. "
            , _cardImage = "WhiteElephantBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 40 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Basilisk"
            , _cardDescription =
                "At the end of its owner's turn Basilisk deals 4 damage to each of the opponent's creatures with 8 or less life. Activated ability (cost 0) Deals 6 damage to target creature. "
            , _cardImage = "BasiliskBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 54 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ancient Dragon"
            , _cardDescription =
                "When Ancient Dragon is summoned it increases each of its owner's powers by 1. Activated ability (cost 3) Ancient Dragon deals 10 damage to the opponent and to each of the opponent's creatures. "
            , _cardImage = "AncientDragonBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 45 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "BEAST'S ABILITIES"
      , [ Card
            { _cardName = "Trumpet"
            , _cardDescription =
                "Elephant's ability, cost 0 White Elephant increases its owner's Beast power by 1. "
            , _cardImage = "TrumpetBig.jpg"
            , _cardCost = 0
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Gaze"
            , _cardDescription =
                "Basilisk's ability, cost 0 Deals 6 damage to target creature. "
            , _cardImage = "GazeBig.jpg"
            , _cardCost = 0
            , _cardEffect = Spell
            , _cardTarget = TargetCreature }
        , Card
            { _cardName = "Pump Energy"
            , _cardDescription =
                "Energy Beast increases its owner's Fire, Water, Air and Earth powers by 1. "
            , _cardImage = "PumpEnergyBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget =  NoTarget }
        , Card
            { _cardName = "Move Falcon"
            , _cardDescription =
                "Death Falcon moves to target empty slot and deals 4 damage to each of the opponent's creatures. "
            , _cardImage = "MoveFalconBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Poison"
            , _cardDescription =
                "Scorpions's ability, cost 2 Scorpion deals 14 damage to target opponent's creature. "
            , _cardImage = "PoisonBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = TargetOpponent's }
        , Card
            { _cardName = "Enrage"
            , _cardDescription =
                "Wolverine completely heals itself and its attack is increased by 2 permanently. "
            , _cardImage = "EnrageBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Natural Healing"
            , _cardDescription =
                "Magic Hamster heals 18 life to all of its owner's creatures. "
            , _cardImage = "NaturalHealingBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Breathe Fire"
            , _cardDescription =
                "Ancient Dragon deals 10 damage to the opponent and to each of the opponent's creatures. "
            , _cardImage = "BreatheFireBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        ]
      )
    , ( "CHAOS CARDS"
      , [ Card
            { _cardName = "Insanian Peacekeeper"
            , _cardDescription =
                "At the beginning of its owner's turn, Insanian Peacekeeper heals 1-6 life to its owner. "
            , _cardImage = "InsanianPeacekeeperBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 11 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Insanian Berserker"
            , _cardDescription =
                "At the beginning of its owner's turn, Insanian Berseker deals 1-6 damage to the opponent. "
            , _cardImage = "InsanianBerserkerBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 14 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Doom Bolt"
            , _cardDescription =
                "Chaos spell, cost 3 Doom Bolt deals 25 damage to a random opponent's creature. "
            , _cardImage = "DoomBoltBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Chaotic Wave"
            , _cardDescription =
                "Chaotic Wave deals 2-12 damage to each of the opponent's creatures, then heals 2-12 life to each of its caster's creatures. "
            , _cardImage = "ChaoticWaveBig.jpg"
            , _cardCost = 4
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Insanian Shaman"
            , _cardDescription =
                "At the beginning of its owner's turn, Insanian Shaman decreases a random opponent's power by 2. "
            , _cardImage = "InsanianShamanBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 25 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Insanian Lord"
            , _cardDescription =
                "At the beginning of its owner's turn, Insanian Lord increases a random owner's power by 2. "
            , _cardImage = "InsanianLordBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 28 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Insanian Catapult"
            , _cardDescription =
                "At the beginning of its owner's turn, Insanian Catapult deals 10 damage to a random opponent's creature. "
            , _cardImage = "InsanianCatapultBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 38 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Insanian King"
            , _cardDescription =
                "At the end of its owner's turn, Insanian King puts a 4/15 Soldier into a random empty slot. "
            , _cardImage = "InsanianKingBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 46 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "CONTROL CARDS"
      , [ Card
            { _cardName = "Goblin Shaman"
            , _cardDescription =
                "Goblin Shaman increases the cost of all of the opponent's spells by 1. "
            , _cardImage = "GoblinShamanBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 12 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Weakness"
            , _cardDescription =
                "Weakness decreases each of the opponent's powers by 1, then deals 3 damage to the opponent. "
            , _cardImage = "WeaknessBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Damping Tower"
            , _cardDescription =
                "Damping Tower increases the cost of all of the opponent's cards by 1. "
            , _cardImage = "DampingTowerBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 0 , _creatureLife = 17 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ancient Horror"
            , _cardDescription =
                "During the opponent's attack, each of their creatures with cost less than Ancient Horror's owner's Control power skip their attack. "
            , _cardImage = "AncientHorrorBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 25 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Poisonous Cloud"
            , _cardDescription =
                "Poisonous Cloud decreases all of the opponent's powers by 1, then deals damage to each of the opponent's creatures equal to half of its life (rounded up). "
            , _cardImage = "PoisonousCloudBig.jpg"
            , _cardCost = 5
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Ancient Witch"
            , _cardDescription =
                "When Ancient Witch is summoned, it decreases each of the opponent's powers by 2. "
            , _cardImage = "AncientWitchBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 18 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Mindstealer"
            , _cardDescription =
                "Each time a creature in the opposing slot performs its attack, it deals damage to itself instead of Mindstealer. "
            , _cardImage = "MindstealerBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 36 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ancient Giant"
            , _cardDescription =
                "When Ancient Giant is summoned it forces opponent to skip their next turn. "
            , _cardImage = "AncientGiantBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 49 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "CULT CARDS"
      , [ Card
            { _cardName = "Fanatic"
            , _cardDescription =
                "Fanatic receives double damage from the opponent's spells. "
            , _cardImage = "FanaticBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 26 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Call to Ancient Spirits"
            , _cardDescription =
                "Call to Ancient Spirits deals 10 damage to each of the opponent's creatures, then deals 5 damage to each of its caster's creatures. "
            , _cardImage = "CalltoAncientSpiritsBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Zealot"
            , _cardDescription =
                "At the beginning of its owner's turn, Zealot's attack increases by 2 permanently. Each time Zealot deals damage it loses an equal amount of life. "
            , _cardImage = "ZealotBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 42 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Monument to Rage"
            , _cardDescription =
                "Owner's creatures perform an additional attack each turn. Each time an owner's creature deal damage by attacking, Monument to Rage loses an equal amount of life. "
            , _cardImage = "MonumenttoRageBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 0 , _creatureLife = 63 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Cursed Unicorn"
            , _cardDescription =
                "While Cursed Unicorn is blocked, it lose 5 life each turn but all spell damage done to Cursed Unicorn is redirected to the opposite creature. "
            , _cardImage = "CursedUnicornBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 53 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Blind Prophet"
            , _cardDescription =
                "Blind Prophet increases the growth of its owner's Fire, Water, Air and Earth powers by 1 and decreases the growth of its owner's Worship power by 1. "
            , _cardImage = "BlindProphetBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 27 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Reaver"
            , _cardDescription =
                "All damage that would be dealt to its owner's other creatures is redirected to Reaver instead. While Reaver is in play, its owner can't cast spells. "
            , _cardImage = "ReaverBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 56 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Greater Bargul"
            , _cardDescription =
                "When Greater Bargul is summoned it deals 20 damage to each other creature. At the beginning of its owner's turn Greater Bargul deals 3 damage to its owner. "
            , _cardImage = "GreaterBargulBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 13 , _creatureLife = 58 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "DEATH CARDS"
      , [ Card
            { _cardName = "Dark Ritual"
            , _cardDescription =
                "Dark Ritual deals 3 damage to the opponent's creatures, then heals 3 life to each of its caster's creatures. "
            , _cardImage = "DarkRitualBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Cursed Fog"
            , _cardDescription =
                "Cursed Fog deals 12 damage to each creature and 3 damage to the opponent. "
            , _cardImage = "CursedFogBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Banshee"
            , _cardDescription =
                "When Banshee is summoned it deals damage to the creature in the opposing slot equal to half of that creature's life (rounded up). "
            , _cardImage = "BansheeBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 21 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Emissary of Dorlak"
            , _cardDescription =
                "Emissary of Dorlak must be summoned onto another of its owner's creatures. That creature is destroyed. "
            , _cardImage = "EmissaryofDorlakBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 48 }
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Blood Ritual"
            , _cardDescription =
                "Destroy target caster's creature. Blood Ritual deals X damage to each of the opponent's creatures, where X is equal to the life of the destroyed creature, but not more than 32. "
            , _cardImage = "BloodRitualBig.jpg"
            , _cardCost = 5
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Keeper of Death"
            , _cardDescription =
                "Each time an opponent's creature dies Keeper of Death increases its owner's Death power by 1. "
            , _cardImage = "KeeperofDeathBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 35 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Drain Souls"
            , _cardDescription =
                "Kill all creatures. Drain Souls heals an amount of life to its caster equal to twice the number of creatures killed by this spell. When Drain Souls is cast it is replaced by the Rage of Souls spell card. "
            , _cardImage = "DrainSoulsBig.jpg"
            , _cardCost = 7
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Master Lich"
            , _cardDescription =
                "When Master Lich is summoned it deals 8 damage to each of the opponent's creatures. Each time Master Lich deals damage to the opponent, it increases its owner's Death power by 2. "
            , _cardImage = "MasterLichBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 46 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "DEMONIC CARDS"
      , [ Card
            { _cardName = "Lemure"
            , _cardDescription =
                "When Lemure dies it is replaced by a 3/6 Scrambled Lemure. "
            , _cardImage = "LemureBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 8 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Explosion"
            , _cardDescription =
                "Destroy target caster's creature. Explosion deals 28 damage to the creature in the opposing slot. "
            , _cardImage = "ExplosionBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Power Chains"
            , _cardDescription =
                "Power Chains deals 12 damage to target opponent's Fire, Water, Air or Earth Creature, then decreases that opponent's power by 3. "
            , _cardImage = "PowerChainsBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = TargetOpponent'sNormal }
        , Card
            { _cardName = "Ergodemon"
            , _cardDescription =
                "When Ergodemon dies, it decreases each of the opponent's powers by 1. "
            , _cardImage = "ErgodemonBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 23 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Demon Quartermaster"
            , _cardDescription =
                "Demon Quartermaster increases the growth of its owner's Demonic power by 1. When Demon Quartermaster dies it is replaced by a 6/20 Enraged Quartermaster. "
            , _cardImage = "DemonQuartermasterBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 21 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "HellFire"
            , _cardDescription =
                "Hellfire deals 13 damage to each of the opponent's creatures, then increases its caster's Fire power by 1 for each creature killed by this spell. "
            , _cardImage = "HellFireBig.jpg"
            , _cardCost = 6
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Three-headed demon"
            , _cardDescription =
                "Three-headed Demon's attack damages the opponent and each of the opponent's creatures. When Three-headed Demon dies it is replaced by a 2/25 Demon Apostate. "
            , _cardImage = "ThreeheadeddemonBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 30 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Greater Demon"
            , _cardDescription =
                "When Greater Demon is summoned it deals an amount of damage to the opponent and to each of the opponent's creatures equal to its owner's Fire power, but not more than 10. "
            , _cardImage = "GreaterDemonBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 42 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "EARTH CARDS"
      , [ Card
            { _cardName = "Elven Healer"
            , _cardDescription =
                "At the beginning of its owner's turn, Elvish Healer heals 3 life to its owner. "
            , _cardImage = "ElvenHealerBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 12 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Nature's Ritual"
            , _cardDescription =
                "Nature's Ritual heals 8 life to target caster's creature and to its caster. "
            , _cardImage = "NaturesRitualBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Forest Sprite"
            , _cardDescription =
                "Forest Sprite's attack damages the opponent and each of the opponent's creatures. "
            , _cardImage = "ForestSpriteBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 1 , _creatureLife = 22 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Rejuvenation"
            , _cardDescription =
                "Rejuvenation heals an amount of life to its caster equal to twice its caster's Earth power. "
            , _cardImage = "RejuvenationBig.jpg"
            , _cardCost = 4
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Elf Hermit"
            , _cardDescription =
                "Elf Hermit increases the growth of its owner's Earth power by 2. "
            , _cardImage = "ElfHermitBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 1 , _creatureLife = 13 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Nature's Fury"
            , _cardDescription =
                "Nature's Fury deals an amount of damage to the opponent equal to the total attack of its caster's two creatures with the highest attack. "
            , _cardImage = "NaturesFuryBig.jpg"
            , _cardCost = 6
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Giant Spider"
            , _cardDescription =
                "When Giant Spider is summoned, it puts a 2/11 Forest Spider into each of its empty neighboring slots. "
            , _cardImage = "GiantSpiderBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 24 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Troll"
            , _cardDescription =
                "At the beginning of its owner's turn, Troll heals 4 life to itself. "
            , _cardImage = "TrollBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 26 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Stone Rain"
            , _cardDescription =
                "Earth spell, cost 9 Stone Rain deals 25 damage to each creature. "
            , _cardImage = "StoneRainBig.jpg"
            , _cardCost = 9
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Earth Elemental"
            , _cardDescription =
                "Attack is equal to its owners Earth power. Earth Elemental increases the growth of its owner's Earth power by 1. "
            , _cardImage = "EarthElementalBig.jpg"
            , _cardCost = 10
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Nothing , _creatureLife = 50 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Master Healer"
            , _cardDescription =
                "At the beginning of its owner's turn, Master Healer heals 3 life to its owner and to each of its owner's creatures. "
            , _cardImage = "MasterHealerBig.jpg"
            , _cardCost = 11
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 34 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Hydra"
            , _cardDescription =
                "Hydra's attack damages the opponent and each of the opponent's creatures. At the beginning of its owner's turn, Hydra heals 4 life to itself. "
            , _cardImage = "HydraBig.jpg"
            , _cardCost = 12
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 40 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "FIRE CARDS"
      , [ Card
            { _cardName = "Goblin Berserker"
            , _cardDescription =
                "At the beginning of its owners turn, Goblin Berserker deals 2 damage to its neighboring creatures. "
            , _cardImage = "GoblinBerserkerBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 16 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Wall of Fire"
            , _cardDescription =
                "When Wall of Fire is summoned it deals 5 damage to each of the opponent's creatures. "
            , _cardImage = "WallofFireBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 0 , _creatureLife = 5 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Priest of Fire"
            , _cardDescription =
                "Priest of Fire increases the growth of its owner's Fire power by 1. "
            , _cardImage = "PriestofFireBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 13 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Fire Drake"
            , _cardDescription = "Fire Drake attacks the turn it enters play. "
            , _cardImage = "FireDrakeBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 18 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Orc Chieftain"
            , _cardDescription =
                "Orc Chieftan increases the attack of its neighboring creatures by 2 (except walls). "
            , _cardImage = "OrcChieftainBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 17 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Flame Wave"
            , _cardDescription =
                "Fire spell, cost 6 Flame Wave deals 9 damage to each of the opponent's creatures. "
            , _cardImage = "FlameWaveBig.jpg"
            , _cardCost = 6
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Minotaur Commander"
            , _cardDescription =
                "Minotaur Commander increases by 1 the attack of all other owner's creatures (except walls). "
            , _cardImage = "MinotaurCommanderBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 20 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Bargul"
            , _cardDescription =
                "When Bargul is summoned it deals 4 damage to each other creature. "
            , _cardImage = "BargulBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 26 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Inferno"
            , _cardDescription =
                "Inferno deals 18 damage to target opponent's creature and 10 damage to each of the opponent's other creatures. "
            , _cardImage = "InfernoBig.jpg"
            , _cardCost = 9
            , _cardEffect = Spell
            , _cardTarget = TargetOpponent's }
        , Card
            { _cardName = "Fire Elemental"
            , _cardDescription =
                "Attack is equal to its owner's fire power. When Fire Elemental is summoned it deals 3 damage to the opponent and to each of the opponent's creatures. Fire Elemental increases the growth of its owner's Fire power by 1. "
            , _cardImage = "FireElementalBig.jpg"
            , _cardCost = 10
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Nothing , _creatureLife = 37 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Armageddon"
            , _cardDescription =
                "Armageddon deals (8 + its caster's Fire power) damage to the opponent and to each creature. "
            , _cardImage = "ArmageddonBig.jpg"
            , _cardCost = 11
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Dragon"
            , _cardDescription =
                "Dragon increases the damage its owner's spells deal by 50% (total damage will be rounded up). "
            , _cardImage = "DragonBig.jpg"
            , _cardCost = 12
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 9 , _creatureLife = 40 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "FOREST CARDS"
      , [ Card
            { _cardName = "Crazy Squirrel"
            , _cardDescription =
                "When Crazy Squirrel is summoned it deals 8 damage to the creature in the opposing slot. "
            , _cardImage = "CrazySquirrelBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 2 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Forest Wolf"
            , _cardDescription =
                "Magic Rabbit. Magic Rabbit is killed. The attack of Forest Wolf is equal to the attack of the killed Rabbit. "
            , _cardImage = "ForestWolfBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 0 , _creatureLife = 24 }
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Vindictive Raccoon"
            , _cardDescription =
                "When Vindictive Raccoon is summoned it deals damage to the opponent equal to the attack of the creature in the opposing slot. "
            , _cardImage = "VindictiveRaccoonBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 14 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Enraged Beaver"
            , _cardDescription =
                "When Enraged Beaver is summoned it deals X damage to the opponent and to each of the opponent's creatures where X is equal to the attack of Magic Rabbit. "
            , _cardImage = "EnragedBeaverBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 10 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ritual of the Forest"
            , _cardDescription =
                "Ritual of the Forest heals X+5 life to its caster and to each of its caster's creatures where X is equal to the attack of Magic Rabbit. "
            , _cardImage = "RitualoftheForestBig.jpg"
            , _cardCost = 5
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Treefolk Protector"
            , _cardDescription =
                "Treefolk Protector decreases by 3 damage dealt by the opponent's spells. "
            , _cardImage = "TreefolkProtectorBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 25 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Bee Queen"
            , _cardDescription =
                "When Bee Queen is summoned it puts a 2/8 Bee Soldier into each of its empty neighbouring slots. When Bee Queen or a Bee Soldier dies it deals 3 damage to the opponent. "
            , _cardImage = "BeeQueenBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 14 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Angry Angry Bear"
            , _cardDescription =
                "Each time Angry Angry Bear receives damage its attack increases by 1 permanently. "
            , _cardImage = "AngryAngryBearBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 30 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "GOBLIN'S CARDS"
      , [ Card
            { _cardName = "Rescue operation"
            , _cardDescription =
                "Move target creature to a random empty slot. If that creature is owned by this spell's caster, Rescue Operation heals 5 life to it. "
            , _cardImage = "RescueoperationBig.jpg"
            , _cardCost = 0
            , _cardEffect = Spell
            , _cardTarget = TargetCreature }
        , Card
            { _cardName = "Goblin Hero"
            , _cardDescription =
                "Goblin Hero's attack is increased by 2 for each neighbouring creature. Each time the opponent summons a creature in the opposing slot, Goblin Hero moves to a random empty slot. "
            , _cardImage = "GoblinHeroBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 14 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Goblin Saboteur"
            , _cardDescription =
                "Each time Goblin Saboteur deals damage to the opponent, the opponent loses the cheapest card of a random power type. "
            , _cardImage = "GoblinSaboteurBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 20 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Army of Rats"
            , _cardDescription =
                "Army of Rats deals 12 damage to each of the opponent's creatures, then deals 12 damage to a random caster's creature. "
            , _cardImage = "ArmyofRatsBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Portal Jumper"
            , _cardDescription =
                "At the end of its owner's turn Portal Jumper stuns the creature in the opposing slot, preventing it from attacking for one turn, then moves to a random slot. "
            , _cardImage = "PortalJumperBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 28 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Goblin Looter"
            , _cardDescription =
                "Each time any creature dies Goblin Looter increases a random power of its owner by 1. "
            , _cardImage = "GoblinLooterBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 28 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Goblin Raider"
            , _cardDescription =
                "When Goblin Raider is summoned, it puts two other Goblin Raiders into random empty slots. "
            , _cardImage = "GoblinRaiderBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 24 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ratmaster"
            , _cardDescription =
                "At the beginning of its owner's turn Ratmaster deals 6 damage to each of the opponent's creatures and decreases a random owner's power by 3. "
            , _cardImage = "RatmasterBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 47 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "GOLEM CARDS"
      , [ Card
            { _cardName = "Golem's Frenzy"
            , _cardDescription =
                "Golem's Frenzy deals 3 damage to each of the opponent's creatures. For each creature killed by this spell, Golem's attack is increased by 3 until end of turn. "
            , _cardImage = "GolemsFrenzyBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Guardian Statue"
            , _cardDescription =
                "Guardian Statue receives no damage until it performs its first attack. "
            , _cardImage = "GuardianStatueBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 10 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Golem's Justice"
            , _cardDescription =
                "Golem's Justice deals 4 damage to each of the opponent's creatures, then heals 4 life to Golem's neighbours. "
            , _cardImage = "GolemsJusticeBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Army Upgrade"
            , _cardDescription =
                "Army Upgrade heals 3 life to each of its caster's creatures. Golem's attack is increased by 2 permanently; this bonus remains even after Golem is killed. "
            , _cardImage = "ArmyUpgradeBig.jpg"
            , _cardCost = 4
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Golem Instructor"
            , _cardDescription =
                "Golem Instructor increases Golem's attack by 2. "
            , _cardImage = "GolemInstructorBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Nothing , _creatureLife = 22 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Dark Sculptor"
            , _cardDescription =
                "When Dark Sculptor is summoned it deals X damage to each of the opponent's creatures, where X is equal to the number of creatures in play. "
            , _cardImage = "DarkSculptorBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 8 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Golem Guide"
            , _cardDescription =
                "When Golem Guide is summoned it swaps positions with Golem and increases Golem's attack by 3 until end of turn. "
            , _cardImage = "GolemGuideBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 32 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Golem Handler"
            , _cardDescription =
                "While Golem Handler is in play, Golem performs an additional attack during each of its owner's turns. "
            , _cardImage = "GolemHandlerBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 40 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "HOLY CARDS"
      , [ Card
            { _cardName = "Paladin"
            , _cardDescription =
                "When Paladin is summoned it heals 4 life to each of its owner's creatures. "
            , _cardImage = "PaladinBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 9 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Monk"
            , _cardDescription =
                "When Monk dies it increases its owner's Holy power by 2. "
            , _cardImage = "MonkBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 13 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Holy Guard"
            , _cardDescription =
                "Holy Guard decreases all damage dealt to its neighboring creatures by 2. "
            , _cardImage = "HolyGuardBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 23 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Divine Justice"
            , _cardDescription =
                "Divine Justice heals 12 life to target caster's creature and deals 12 damage to each other creature. "
            , _cardImage = "DivineJusticeBig.jpg"
            , _cardCost = 4
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Divine Intervention"
            , _cardDescription =
                "Divine Intervention increases its caster's Fire, Water, Air and Earth powers by 2, then heals 10 life to its caster. "
            , _cardImage = "DivineInterventionBig.jpg"
            , _cardCost = 5
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Wrath of God"
            , _cardDescription =
                "Wrath of God deals 12 damage to each of the opponent's creatures, then increases its caster's Holy power by 1 for each of the opponent's creatures that did not die. "
            , _cardImage = "WrathofGodBig.jpg"
            , _cardCost = 6
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Angel"
            , _cardDescription =
                "When Angel is summoned it increases its owner's Holy power by 3. "
            , _cardImage = "AngelBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 42 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Archangel"
            , _cardDescription =
                "When Archangel is summoned it completely heals each of its owner's creatures. "
            , _cardImage = "ArchangelBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 48 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "ILLUSION CARDS"
      , [ Card
            { _cardName = "Madness"
            , _cardDescription =
                "Madness deals damage to each of the opponent's creatures equal to the attack of that creature. "
            , _cardImage = "MadnessBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Phantom Warrior"
            , _cardDescription =
                "Damage dealt to Phantom Warrior is reduced to 1. "
            , _cardImage = "PhantomWarriorBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 4 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Hypnosis"
            , _cardDescription =
                "The two opponent's creatures with the highest attack immediately attack the opponent. "
            , _cardImage = "HypnosisBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Wall of Reflection"
            , _cardDescription =
                "Each time Wall of Reflection receives damage it deals an equal amount of damage to the opponent, but not more than the life lost by Wall of Reflection. "
            , _cardImage = "WallofReflectionBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 0 , _creatureLife = 20 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Spectral Assassin"
            , _cardDescription =
                "When Spectral Assassin is summoned it deals 12 damage to the opponent. "
            , _cardImage = "SpectralAssassinBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 22 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Spectral Mage"
            , _cardDescription =
                "When Spectral Mage is summoned it deals damage to each of the opponent's creatures equal to the cost of that creature. "
            , _cardImage = "SpectralMageBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 34 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Oracle"
            , _cardDescription =
                "At the beginning of its owner's turn, Oracle deals damage to the opponent equal to its owner's Illusion power. "
            , _cardImage = "OracleBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 41 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Hypnotist"
            , _cardDescription =
                "When Hypnotist is summoned it deals 5 damage to the opponent and to each of the opponent's creatures. Hypnotist increases the growth of its owner's Illusion power by 1. "
            , _cardImage = "HypnotistBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 39 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "MECHANICAL CARDS"
      , [ Card
            { _cardName = "Overtime"
            , _cardDescription =
                "Mechanical spell, cost 0 Overtime increases its caster's Mechanics power by 1. "
            , _cardImage = "OvertimeBig.jpg"
            , _cardCost = 0
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Dwarven Rifleman"
            , _cardDescription =
                "Each time the opponent summons a creature Dwarven Rifleman deals 4 damage to that creature. "
            , _cardImage = "DwarvenRiflemanBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 17 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Dwarven Craftsman"
            , _cardDescription =
                "Dwarven Craftsman increases the growth of its owner's Mechanics power by 1. "
            , _cardImage = "DwarvenCraftsmanBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 17 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ornithopter"
            , _cardDescription =
                "At the beginning of its owner's turn, Ornithopter deals 2 damage to each of the opponent's creatures. "
            , _cardImage = "OrnithopterBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 24 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Steel Golem"
            , _cardDescription =
                "Steel Golem receives no damage from spells and abilities. Damage dealt to Steel Golem is reduced by 1. "
            , _cardImage = "SteelGolemBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 20 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Cannon"
            , _cardDescription =
                "At the beginning of its owner's turn, Cannon deals 8 damage to the opponent's creature with the most life. "
            , _cardImage = "CannonBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 29 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Cannonade"
            , _cardDescription =
                "Cannonade deals 19 damage to each of the opponent's creatures. "
            , _cardImage = "CannonadeBig.jpg"
            , _cardCost = 7
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Steam Tank"
            , _cardDescription =
                "When Steam Tank is summoned it deals 12 damage to each of the opponent's creatures. "
            , _cardImage = "SteamTankBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 52 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "OTHER CARDS"
      , [ Card
            { _cardName = "Forest Spider"
            , _cardDescription = "Spiders."
            , _cardImage = "ForestSpiderBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 11 }
            , _cardTarget = TargetCasterBlank }
      ,  Card
            { _cardName = "Soldier"
            , _cardDescription = "No abilities."
            , _cardImage = "ChaosSoldierBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4, _creatureLife = 15 }
            , _cardTarget = TargetCasterBlank }
      ,  Card
            { _cardName = "Bee Soldier"
            , _cardDescription =
                "When Bee Soldier dies it deals 3 damage to opponent. "
            , _cardImage = "BeeSoldierBig.jpg"
            , _cardCost = 0
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 8 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Initiate"
            , _cardDescription =
                "While Vampire Elder is in play Initiates receive no damage. "
            , _cardImage = "InitiateBig.jpg"
            , _cardCost = 0
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 14 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Scrambled Lemure"
            , _cardDescription = "Attack 3, life 6 "
            , _cardImage = "ScrambledLemureBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 6 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Magic Rabbit"
            , _cardDescription =
                "Each turn Mad Hermit summons Magic Rabbit as a passive class ability. There can be only one Magic Rabbit in play. At the beginning of its owner's turn Magic Rabbit's attack increases by 1 permanently. "
            , _cardImage = "MagicRabbitBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 1 , _creatureLife = 10 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Golem"
            , _cardDescription =
                "Golem receives no damage from spells and abilities. When Golem dies Golem is put into another random empty slot (with full life) and its owner loses 10 life. "
            , _cardImage = "GolemBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 10 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Enraged Quartermaster"
            , _cardDescription = "Attack 6, life 20 "
            , _cardImage = "EnragedQuartermasterBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 20 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Demon Apostate"
            , _cardDescription =
                "At the beginning of its owner's turn Demon Apostate heals 2 life to its owner and to each of its owner's creatures. "
            , _cardImage = "DemonApostateBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 2 , _creatureLife = 25 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Rage of Souls"
            , _cardDescription =
                "Deals (9 + caster's Death power) damage to each of opponent's creatures. Caster gains 2 life for each creature killed by this spell. "
            , _cardImage = "RageofSoulsBig.jpg"
            , _cardCost = 7
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        ]
      )
    , ( "SORCERY CARDS"
      , [ Card
            { _cardName = "Healing Spray"
            , _cardDescription =
                "Healing Spray heals 9 life to target caster's creature and 6 life to its neighboring creatures. "
            , _cardImage = "HealingSprayBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Fireball"
            , _cardDescription =
                "Fireball deals 9 damage to target opponent's creature and 6 damage to its neighboring creatures. "
            , _cardImage = "FireballBig.jpg"
            , _cardCost = 2
            , _cardEffect = Spell
            , _cardTarget = TargetOpponent's }
        , Card
            { _cardName = "Steal Essence"
            , _cardDescription =
                "Steal Essence deals 5 damage to target creature. If the creature is killed by this spell, its caster's Sorcery power is increased by 4. "
            , _cardImage = "StealEssenceBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = TargetCreature }
        , Card
            { _cardName = "Sacrifice"
            , _cardDescription =
                "Destroy target caster's creature. Sacrifice increases its caster's Fire, Water, Air and Earth powers by 3. "
            , _cardImage = "SacrificeBig.jpg"
            , _cardCost = 4
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Ritual of Glory"
            , _cardDescription =
                "Ritual of Glory completely heals each of its caster's creatures, then increases their attack by 3 until end of turn (except walls). "
            , _cardImage = "RitualofGloryBig.jpg"
            , _cardCost = 5
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Mana Burn"
            , _cardDescription =
                "Mana Burn deals an amount of damage to each of the opponent's creatures equal to the opponent's highest power type, then decreases that opponent's power by 3. "
            , _cardImage = "ManaBurnBig.jpg"
            , _cardCost = 6
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Sonic Boom"
            , _cardDescription =
                "Sonic Boom deals 11 damage to the opponent and to each of the opponent's creatures. Those creatures skip their attack next turn. "
            , _cardImage = "SonicBoomBig.jpg"
            , _cardCost = 7
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Disintegrate"
            , _cardDescription =
                "Destroy target opponent's creature. Disintegrate deals 11 damage to each of the opponent's other creatures. "
            , _cardImage = "DisintegrateBig.jpg"
            , _cardCost = 8
            , _cardEffect = Spell
            , _cardTarget = TargetOpponent's }
        ]
      )
    , ( "SPIRIT CARDS"
      , [ Card
            { _cardName = "Crusader"
            , _cardDescription =
                "When Crusader is summoned it heals 2 life to each of its owner's creatures. "
            , _cardImage = "CrusaderBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 15 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Holy Avenger"
            , _cardDescription =
                "Each time a neighboring creature dies, Holy Avenger's attack increases by 2 permanently. "
            , _cardImage = "HolyAvengerBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 23 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Templar"
            , _cardDescription =
                "Each time its owner summons a creature into a neighboring slot, Templar deals 4 damage to the opponent. "
            , _cardImage = "TemplarBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 26 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Divine Justice"
            , _cardDescription =
                "Divine Justice heals 12 life to target caster's creature and deals 12 damage to each other creature. "
            , _cardImage = "DivineJusticeBig.jpg"
            , _cardCost = 4
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Divine Meddling"
            , _cardDescription =
                "Divine Meddling increases its caster's Fire, Water, Air and Earth powers by 2, then deals 10 damage to the opponent. "
            , _cardImage = "DivineMeddlingBig.jpg"
            , _cardCost = 5
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Rage of God"
            , _cardDescription =
                "Rage of God deals 12 damage to each of the opponent's creatures, then deals 3 damage to the opponent for each of the opponent's creatures that did not die. "
            , _cardImage = "RageofGodBig.jpg"
            , _cardCost = 6
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Angel"
            , _cardDescription =
                "When Angel is summoned it increases its owner's Holy power by 3. "
            , _cardImage = "AngelBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 42 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Angel of War"
            , _cardDescription =
                "When Angel of War is summoned it deals 8 damage to each of the opponent's creatures and heals 8 life to each of its owner's creatures. "
            , _cardImage = "AngelofWarBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 37 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "TIME CARDS"
      , [ Card
            { _cardName = "Chrono Hunter"
            , _cardDescription =
                "Each time Chrono Hunter kills an opponent's creature it increases its owner's Time power by 2. "
            , _cardImage = "ChronoHunterBig.jpg"
            , _cardCost = 1
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 11 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Timeblazer"
            , _cardDescription =
                "When Timeblazer is summoned each of its neighbouring creatures attack the opponent this turn instead of the creature in the opposing slot. "
            , _cardImage = "TimeblazerBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 17 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Hasten"
            , _cardDescription =
                "Target caster's creature with attack 9 or less immediately attacks the opponent and all the opponent's creatures. "
            , _cardImage = "HastenBig.jpg"
            , _cardCost = 3
            , _cardEffect = Spell
            , _cardTarget = TargetCaster's }
        , Card
            { _cardName = "Time Stop"
            , _cardDescription =
                "Time Stop deals 4 damage to each of the opponent's creatures and forces the opponent to skip their next turn. "
            , _cardImage = "TimeStopBig.jpg"
            , _cardCost = 4
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Timeweaver"
            , _cardDescription =
                "When Timeweaver is summoned its owner is able to cast an additional spell this turn. Timeweaver decreases the cost of its owner's spells by 1. "
            , _cardImage = "TimeweaverBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 24 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Priestess of Moments"
            , _cardDescription =
                "Priestess of Moments performs an additional attack in each of its owner's turns, and allows its owner's other creatures to attack the turn they enter play. "
            , _cardImage = "PriestessofMomentsBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 35 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Chrono Engine"
            , _cardDescription =
                "Chrono Engine allows its owner to use an additional card each turn. Each time its owner skips a turn Chrono Engine increases its owner's Time power by 1. "
            , _cardImage = "ChronoEngineBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 33 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Time Dragon"
            , _cardDescription =
                "When Time Dragon is summoned its owner may use an additional card this turn. Time Dragon attacks the turn it enters play. "
            , _cardImage = "TimeDragonBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 40 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "VAMPIRIC CARDS"
      , [ Card
            { _cardName = "Blood Boil"
            , _cardDescription =
                "Blood Boil deals 4 damage to each of the opponent's creatures, then increases its owner's Blood power by 1 for each creature killed by this spell. "
            , _cardImage = "BloodBoilBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Ghoul"
            , _cardDescription =
                "Each time an opponent's creature dies Ghoul's attack increases by 1 permanently. "
            , _cardImage = "GhoulBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 25 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Devoted Servant"
            , _cardDescription =
                "At the beginning of its owner's turn, Devoted Servant's attack increases by 1 permanently. When Devoted Servant dies it increases its owner's Blood power by an amount equal to its attack. "
            , _cardImage = "DevotedServantBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 1 , _creatureLife = 19 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Vampire Mystic"
            , _cardDescription =
                "Each time the opponent receives damage, Vampire Mystic's attack increases by 2 until end of turn. "
            , _cardImage = "VampireMysticBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 46 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Justicar"
            , _cardDescription =
                "The opposing creature receives double damage from all sources. While Justicar is unblocked all damage done to the opponent is increased by 2. "
            , _cardImage = "JusticarBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 49 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Chastiser"
            , _cardDescription =
                "Each time its owner receives damage Chastiser's attack increases by 2 until after its next attack. "
            , _cardImage = "ChastiserBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 9 , _creatureLife = 51 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Vampire Elder"
            , _cardDescription =
                "When Vampire Elder is summoned it puts a 5/14 Initiate into each of its empty neighboring slots. While Vampire Elder is in play Initiates receive no damage. "
            , _cardImage = "VampireElderBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 27 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Magister of Blood"
            , _cardDescription =
                "When Magister of Blood is summoned it deals 16 damage to the opponent and to each of the opponent's blocked creatures. "
            , _cardImage = "MagisterofBloodBig.jpg"
            , _cardCost = 8
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 8 , _creatureLife = 33 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    , ( "WATER CARDS"
      , [ Card
            { _cardName = "Meditation"
            , _cardDescription =
                "Meditation increases its caster's Fire, Air and Earth powers by 1. "
            , _cardImage = "MeditationBig.jpg"
            , _cardCost = 1
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Sea Sprite"
            , _cardDescription =
                "At the beginning of its owner's turn, Sea Sprite deals 2 damage to its owner. "
            , _cardImage = "SeaSpriteBig.jpg"
            , _cardCost = 2
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 22 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Merfolk Apostate"
            , _cardDescription =
                "When Merfolk Apostate is summoned it increases its owner's Fire power by 2. "
            , _cardImage = "MerfolkApostateBig.jpg"
            , _cardCost = 3
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 10 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ice Golem"
            , _cardDescription =
                "Ice Golem receives no damage from spells and abilites. "
            , _cardImage = "IceGolemBig.jpg"
            , _cardCost = 4
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 4 , _creatureLife = 12 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Merfolk Elder"
            , _cardDescription =
                "Merfolk Elder increases the growth of its owner's Air power by 1. "
            , _cardImage = "MerfolkElderBig.jpg"
            , _cardCost = 5
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 16 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Ice Guard"
            , _cardDescription =
                "Ice Guard reduces all damage dealt to its owner by 50% (total damage will be rounded up). "
            , _cardImage = "IceGuardBig.jpg"
            , _cardCost = 6
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 3 , _creatureLife = 20 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Giant Turtle"
            , _cardDescription =
                "Damage dealt to Giant Turtle is reduced by 5. "
            , _cardImage = "GiantTurtleBig.jpg"
            , _cardCost = 7
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 5 , _creatureLife = 16 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Acidic Rain"
            , _cardDescription =
                "Acidic Rain deals 15 damage to each creature, then decreases each of the opponent's powers by 1. "
            , _cardImage = "AcidicRainBig.jpg"
            , _cardCost = 8
            , _cardEffect = Spell
            , _cardTarget = NoTarget }
        , Card
            { _cardName = "Merfolk Overlord"
            , _cardDescription =
                "Merfolk Overlord allows creatures in its neighboring slots to attack the turn they enter play. "
            , _cardImage = "MerfolkOverlordBig.jpg"
            , _cardCost = 9
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 7 , _creatureLife = 35 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Water Elemental"
            , _cardDescription =
                "Attack is equal to its owner's water power.  When Water Elemental is summoned it heals 10 life to its owner. Water Elemental increases the growth of its owner's Water power by 1. "
            , _cardImage = "WaterElementalBig.jpg"
            , _cardCost = 10
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Nothing , _creatureLife = 37 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Mind Master"
            , _cardDescription =
                "Mind Master increases the growth of each of its owner's powers by 1. "
            , _cardImage = "MindMasterBig.jpg"
            , _cardCost = 11
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 6 , _creatureLife = 23 }
            , _cardTarget = TargetCasterBlank }
        , Card
            { _cardName = "Astral Guard"
            , _cardDescription =
                "Astral Guard decreases the growth of each of the opponent's powers by 1. "
            , _cardImage = "AstralGuardBig.jpg"
            , _cardCost = 12
            , _cardEffect =
                Creature
                  CreatureCard { _creatureAttack = Just 1 , _creatureLife = 18 }
            , _cardTarget = TargetCasterBlank }
        ]
      )
    ]
