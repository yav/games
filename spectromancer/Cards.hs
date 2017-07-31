{-# Language OverloadedStrings #-}
module Cards where

import qualified Data.Map as Map
import Types

allCards :: Cards
allCards =
  Map.fromList
    [ ( "AIR CARDS"
      , [ Card
            { cardName = "Faerie Apprentice"
            , cardDescription =
                "Faerie Apprentice increases the damage its owner's spells deal by 1. "
            , cardImage = "FaerieApprenticeBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 12 }
            }
        , Card
            { cardName = "Griffin"
            , cardDescription =
                "When Griffin is summoned, if its owner's Air power is 5 or more, it deals 5 damage to the opponent. "
            , cardImage = "GriffinBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 15 }
            }
        , Card
            { cardName = "Call to Thunder"
            , cardDescription =
                "Call to Thunder deals 6 damage to target opponent's creature and 6 damage to the opponent. "
            , cardImage = "CalltoThunderBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Faerie Sage"
            , cardDescription =
                "When Faerie Sage is summoned it heals an amount of life to its owner equal to its owner's Earth power, but not more than 10. "
            , cardImage = "FaerieSageBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 19 }
            }
        , Card
            { cardName = "Wall of Lightning"
            , cardDescription =
                "At the beginning of its owner's turn, Wall of Lightning deals 4 damage to the opponent. "
            , cardImage = "WallofLightningBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 0 , creatureLife = 28 }
            }
        , Card
            { cardName = "Lightning Bolt"
            , cardDescription =
                "Lightning Bolt deals (5+its caster's Air power) damage to the opponent. "
            , cardImage = "LightningBoltBig.jpg"
            , cardCost = 6
            , cardEffect = Spell
            }
        , Card
            { cardName = "Phoenix"
            , cardDescription =
                "Each time Phoenix dies, if its owner's Fire power is 10 or greater, it rebirths (another Phoenix is put into the same slot). Phoenix cannot rebirth if a card says that it is \"destroyed\". "
            , cardImage = "PhoenixBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 16 }
            }
        , Card
            { cardName = "Chain Lightning"
            , cardDescription =
                "Chain Lightning deals 9 damage to the opponent and to each of the opponent's creatures. "
            , cardImage = "ChainLightningBig.jpg"
            , cardCost = 8
            , cardEffect = Spell
            }
        , Card
            { cardName = "Lightning Cloud"
            , cardDescription =
                "Lightning Cloud's attack damages the opponent and each of the opponent's creatures. "
            , cardImage = "LightningCloudBig.jpg"
            , cardCost = 9
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 20 }
            }
        , Card
            { cardName = "Tornado"
            , cardDescription =
                "Air spell, cost 10 Destroy target opponent's creature. "
            , cardImage = "TornadoBig.jpg"
            , cardCost = 10
            , cardEffect = Spell
            }
        , Card
            { cardName = "Air Elemental"
            , cardDescription =
                "Attack is equal to its owner's air power. When Air Elemental is summoned it deals 8 damage to the opponent. Air Elemental increases the growth of its owner's Air power by 1. "
            , cardImage = "AirElementalBig.jpg"
            , cardCost = 11
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Nothing , creatureLife = 44 }
            }
        , Card
            { cardName = "Titan"
            , cardDescription =
                "When Titan is summoned it deals 15 damage to the creature in the opposing slot. "
            , cardImage = "TitanBig.jpg"
            , cardCost = 12
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 9 , creatureLife = 40 }
            }
        ]
      )
    , ( "BEAST CARDS"
      , [ Card
            { cardName = "Magic Hamster"
            , cardDescription =
                "10 life to each of its neighboring creatures. Activated ability (cost 3) Magic Hamster heals 18 life to all of its owner's creatures. "
            , cardImage = "MagicHamsterBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 9 }
            }
        , Card
            { cardName = "Scorpion"
            , cardDescription =
                "the creature in the opposing slot to skip its attack next turn.'+' Activated ability (cost 2) Scorpion deals 14 damage to target opponent's creature. "
            , cardImage = "ScorpionBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 18 }
            }
        , Card
            { cardName = "Wolverine"
            , cardDescription =
                "Wolverine completely heals itself and its attack is increased by 2 permanently. "
            , cardImage = "WolverineBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 25 }
            }
        , Card
            { cardName = "Energy Beast"
            , cardDescription =
                "Energy Beast increases its owner's Fire, Water, Air and Earth powers by 1. "
            , cardImage = "EnergyBeastBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 34 }
            }
        , Card
            { cardName = "Death Falcon"
            , cardDescription =
                "Death Falcon moves to target empty slot and deals 4 damage to each of the opponent's creatures. "
            , cardImage = "DeathFalconBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 45 }
            }
        , Card
            { cardName = "White Elephant"
            , cardDescription =
                "owner is dealt to White Elephant instead. Activated ability (cost 0) White Elephant increases its owner's Beast power by 1. "
            , cardImage = "WhiteElephantBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 40 }
            }
        , Card
            { cardName = "Basilisk"
            , cardDescription =
                "deals 4 damage to each of the opponent's creatures with 8 or less life. Activated ability (cost 0) Deals 6 damage to target creature. "
            , cardImage = "BasiliskBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 54 }
            }
        , Card
            { cardName = "Ancient Dragon"
            , cardDescription =
                "increases each of its owner's powers by 1. Activated ability (cost 3) Ancient Dragon deals 10 damage to the opponent and to each of the opponent's creatures. "
            , cardImage = "AncientDragonBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 45 }
            }
        ]
      )
    , ( "BEAST'S ABILITIES"
      , [ Card
            { cardName = "Trumpet"
            , cardDescription =
                "Elephant's ability, cost 0 White Elephant increases its owner's Beast power by 1. "
            , cardImage = "TrumpetBig.jpg"
            , cardCost = 0
            , cardEffect = Spell
            }
        , Card
            { cardName = "Gaze"
            , cardDescription =
                "Basilisk's ability, cost 0 Deals 6 damage to target creature. "
            , cardImage = "GazeBig.jpg"
            , cardCost = 0
            , cardEffect = Spell
            }
        , Card
            { cardName = "Pump Energy"
            , cardDescription =
                "Energy Beast increases its owner's Fire, Water, Air and Earth powers by 1. "
            , cardImage = "PumpEnergyBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Move Falcon"
            , cardDescription =
                "Death Falcon moves to target empty slot and deals 4 damage to each of the opponent's creatures. "
            , cardImage = "MoveFalconBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Poison"
            , cardDescription =
                "Scorpions's ability, cost 2 Scorpion deals 14 damage to target opponent's creature. "
            , cardImage = "PoisonBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Enrage"
            , cardDescription =
                "Wolverine completely heals itself and its attack is increased by 2 permanently. "
            , cardImage = "EnrageBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Natural Healing"
            , cardDescription =
                "Magic Hamster heals 18 life to all of its owner's creatures. "
            , cardImage = "NaturalHealingBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Breathe Fire"
            , cardDescription =
                "Ancient Dragon deals 10 damage to the opponent and to each of the opponent's creatures. "
            , cardImage = "BreatheFireBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        ]
      )
    , ( "CHAOS CARDS"
      , [ Card
            { cardName = "Insanian Peacekeeper"
            , cardDescription =
                "At the beginning of its owner's turn, Insanian Peacekeeper heals 1-6 life to its owner. "
            , cardImage = "InsanianPeacekeeperBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 11 }
            }
        , Card
            { cardName = "Insanian Berserker"
            , cardDescription =
                "At the beginning of its owner's turn, Insanian Berseker deals 1-6 damage to the opponent. "
            , cardImage = "InsanianBerserkerBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 14 }
            }
        , Card
            { cardName = "Doom Bolt"
            , cardDescription =
                "Chaos spell, cost 3 Doom Bolt deals 25 damage to a random opponent's creature. "
            , cardImage = "DoomBoltBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Chaotic Wave"
            , cardDescription =
                "Chaotic Wave deals 2-12 damage to each of the opponent's creatures, then heals 2-12 life to each of its caster's creatures. "
            , cardImage = "ChaoticWaveBig.jpg"
            , cardCost = 4
            , cardEffect = Spell
            }
        , Card
            { cardName = "Insanian Shaman"
            , cardDescription =
                "At the beginning of its owner's turn, Insanian Shaman decreases a random opponent's power by 2. "
            , cardImage = "InsanianShamanBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 25 }
            }
        , Card
            { cardName = "Insanian Lord"
            , cardDescription =
                "At the beginning of its owner's turn, Insanian Lord increases a random owner's power by 2. "
            , cardImage = "InsanianLordBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 28 }
            }
        , Card
            { cardName = "Insanian Catapult"
            , cardDescription =
                "At the beginning of its owner's turn, Insanian Catapult deals 10 damage to a random opponent's creature. "
            , cardImage = "InsanianCatapultBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 38 }
            }
        , Card
            { cardName = "Insanian King"
            , cardDescription =
                "At the end of its owner's turn, Insanian King puts a 4/15 Soldier into a random empty slot. "
            , cardImage = "InsanianKingBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 46 }
            }
        ]
      )
    , ( "CONTROL CARDS"
      , [ Card
            { cardName = "Goblin Shaman"
            , cardDescription =
                "Goblin Shaman increases the cost of all of the opponent's spells by 1. "
            , cardImage = "GoblinShamanBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 12 }
            }
        , Card
            { cardName = "Weakness"
            , cardDescription =
                "Weakness decreases each of the opponent's powers by 1, then deals 3 damage to the opponent. "
            , cardImage = "WeaknessBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Damping Tower"
            , cardDescription =
                "Damping Tower increases the cost of all of the opponent's cards by 1. "
            , cardImage = "DampingTowerBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 0 , creatureLife = 17 }
            }
        , Card
            { cardName = "Ancient Horror"
            , cardDescription =
                "During the opponent's attack, each of their creatures with cost less than Ancient Horror's owner's Control power skip their attack. "
            , cardImage = "AncientHorrorBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 25 }
            }
        , Card
            { cardName = "Poisonous Cloud"
            , cardDescription =
                "Poisonous Cloud decreases all of the opponent's powers by 1, then deals damage to each of the opponent's creatures equal to half of its life (rounded up). "
            , cardImage = "PoisonousCloudBig.jpg"
            , cardCost = 5
            , cardEffect = Spell
            }
        , Card
            { cardName = "Ancient Witch"
            , cardDescription =
                "When Ancient Witch is summoned, it decreases each of the opponent's powers by 2. "
            , cardImage = "AncientWitchBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 18 }
            }
        , Card
            { cardName = "Mindstealer"
            , cardDescription =
                "Each time a creature in the opposing slot performs its attack, it deals damage to itself instead of Mindstealer. "
            , cardImage = "MindstealerBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 36 }
            }
        , Card
            { cardName = "Ancient Giant"
            , cardDescription =
                "When Ancient Giant is summoned it forces opponent to skip their next turn. "
            , cardImage = "AncientGiantBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 49 }
            }
        ]
      )
    , ( "CULT CARDS"
      , [ Card
            { cardName = "Fanatic"
            , cardDescription =
                "Fanatic receives double damage from the opponent's spells. "
            , cardImage = "FanaticBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 26 }
            }
        , Card
            { cardName = "Call to Ancient Spirits"
            , cardDescription =
                "Call to Ancient Spirits deals 10 damage to each of the opponent's creatures, then deals 5 damage to each of its caster's creatures. "
            , cardImage = "CalltoAncientSpiritsBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Zealot"
            , cardDescription =
                "At the beginning of its owner's turn, Zealot's attack increases by 2 permanently. Each time Zealot deals damage it loses an equal amount of life. "
            , cardImage = "ZealotBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 42 }
            }
        , Card
            { cardName = "Monument to Rage"
            , cardDescription =
                "Owner's creatures perform an additional attack each turn. Each time an owner's creature deal damage by attacking, Monument to Rage loses an equal amount of life. "
            , cardImage = "MonumenttoRageBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 0 , creatureLife = 63 }
            }
        , Card
            { cardName = "Cursed Unicorn"
            , cardDescription =
                "While Cursed Unicorn is blocked, it lose 5 life each turn but all spell damage done to Cursed Unicorn is redirected to the opposite creature. "
            , cardImage = "CursedUnicornBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 53 }
            }
        , Card
            { cardName = "Blind Prophet"
            , cardDescription =
                "Blind Prophet increases the growth of its owner's Fire, Water, Air and Earth powers by 1 and decreases the growth of its owner's Worship power by 1. "
            , cardImage = "BlindProphetBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 27 }
            }
        , Card
            { cardName = "Reaver"
            , cardDescription =
                "All damage that would be dealt to its owner's other creatures is redirected to Reaver instead. While Reaver is in play, its owner can't cast spells. "
            , cardImage = "ReaverBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 56 }
            }
        , Card
            { cardName = "Greater Bargul"
            , cardDescription =
                "When Greater Bargul is summoned it deals 20 damage to each other creature. At the beginning of its owner's turn Greater Bargul deals 3 damage to its owner. "
            , cardImage = "GreaterBargulBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 13 , creatureLife = 58 }
            }
        ]
      )
    , ( "DEATH CARDS"
      , [ Card
            { cardName = "Dark Ritual"
            , cardDescription =
                "Dark Ritual deals 3 damage to the opponent's creatures, then heals 3 life to each of its caster's creatures. "
            , cardImage = "DarkRitualBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Cursed Fog"
            , cardDescription =
                "Cursed Fog deals 12 damage to each creature and 3 damage to the opponent. "
            , cardImage = "CursedFogBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Banshee"
            , cardDescription =
                "When Banshee is summoned it deals damage to the creature in the opposing slot equal to half of that creature's life (rounded up). "
            , cardImage = "BansheeBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 21 }
            }
        , Card
            { cardName = "Emissary of Dorlak"
            , cardDescription =
                "Emissary of Dorlak must be summoned onto another of its owner's creatures. That creature is destroyed. "
            , cardImage = "EmissaryofDorlakBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 48 }
            }
        , Card
            { cardName = "Blood Ritual"
            , cardDescription =
                "Destroy target caster's creature. Blood Ritual deals X damage to each of the opponent's creatures, where X is equal to the life of the destroyed creature, but not more than 32. "
            , cardImage = "BloodRitualBig.jpg"
            , cardCost = 5
            , cardEffect = Spell
            }
        , Card
            { cardName = "Keeper of Death"
            , cardDescription =
                "Each time an opponent's creature dies Keeper of Death increases its owner's Death power by 1. "
            , cardImage = "KeeperofDeathBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 35 }
            }
        , Card
            { cardName = "Drain Souls"
            , cardDescription =
                "Kill all creatures. Drain Souls heals an amount of life to its caster equal to twice the number of creatures killed by this spell. When Drain Souls is cast it is replaced by the Rage of Souls spell card. "
            , cardImage = "DrainSoulsBig.jpg"
            , cardCost = 7
            , cardEffect = Spell
            }
        , Card
            { cardName = "Master Lich"
            , cardDescription =
                "When Master Lich is summoned it deals 8 damage to each of the opponent's creatures. Each time Master Lich deals damage to the opponent, it increases its owner's Death power by 2. "
            , cardImage = "MasterLichBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 46 }
            }
        ]
      )
    , ( "DEMONIC CARDS"
      , [ Card
            { cardName = "Lemure"
            , cardDescription =
                "When Lemure dies it is replaced by a 3/6 Scrambled Lemure. "
            , cardImage = "LemureBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 8 }
            }
        , Card
            { cardName = "Explosion"
            , cardDescription =
                "Destroy target caster's creature. Explosion deals 28 damage to the creature in the opposing slot. "
            , cardImage = "ExplosionBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Power Chains"
            , cardDescription =
                "Power Chains deals 12 damage to target opponent's Fire, Water, Air or Earth Creature, then decreases that opponent's power by 3. "
            , cardImage = "PowerChainsBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Ergodemon"
            , cardDescription =
                "When Ergodemon dies, it decreases each of the opponent's powers by 1. "
            , cardImage = "ErgodemonBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 23 }
            }
        , Card
            { cardName = "Demon Quartermaster"
            , cardDescription =
                "Demon Quartermaster increases the growth of its owner's Demonic power by 1. When Demon Quartermaster dies it is replaced by a 6/20 Enraged Quartermaster. "
            , cardImage = "DemonQuartermasterBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 2 , creatureLife = 21 }
            }
        , Card
            { cardName = "HellFire"
            , cardDescription =
                "Hellfire deals 13 damage to each of the opponent's creatures, then increases its caster's Fire power by 1 for each creature killed by this spell. "
            , cardImage = "HellFireBig.jpg"
            , cardCost = 6
            , cardEffect = Spell
            }
        , Card
            { cardName = "Three-headed demon"
            , cardDescription =
                "Three-headed Demon's attack damages the opponent and each of the opponent's creatures. When Three-headed Demon dies it is replaced by a 2/25 Demon Apostate. "
            , cardImage = "ThreeheadeddemonBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 30 }
            }
        , Card
            { cardName = "Greater Demon"
            , cardDescription =
                "When Greater Demon is summoned it deals an amount of damage to the opponent and to each of the opponent's creatures equal to its owner's Fire power, but not more than 10. "
            , cardImage = "GreaterDemonBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 42 }
            }
        ]
      )
    , ( "EARTH CARDS"
      , [ Card
            { cardName = "Elven Healer"
            , cardDescription =
                "At the beginning of its owner's turn, Elvish Healer heals 3 life to its owner. "
            , cardImage = "ElvenHealerBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 2 , creatureLife = 12 }
            }
        , Card
            { cardName = "Nature's Ritual"
            , cardDescription =
                "Nature's Ritual heals 8 life to target caster's creature and to its caster. "
            , cardImage = "NaturesRitualBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Forest Sprite"
            , cardDescription =
                "Forest Sprite's attack damages the opponent and each of the opponent's creatures. "
            , cardImage = "ForestSpriteBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 1 , creatureLife = 22 }
            }
        , Card
            { cardName = "Rejuvenation"
            , cardDescription =
                "Rejuvenation heals an amount of life to its caster equal to twice its caster's Earth power. "
            , cardImage = "RejuvenationBig.jpg"
            , cardCost = 4
            , cardEffect = Spell
            }
        , Card
            { cardName = "Elf Hermit"
            , cardDescription =
                "Elf Hermit increases the growth of its owner's Earth power by 2. "
            , cardImage = "ElfHermitBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 1 , creatureLife = 13 }
            }
        , Card
            { cardName = "Nature's Fury"
            , cardDescription =
                "Nature's Fury deals an amount of damage to the opponent equal to the total attack of its caster's two creatures with the highest attack. "
            , cardImage = "NaturesFuryBig.jpg"
            , cardCost = 6
            , cardEffect = Spell
            }
        , Card
            { cardName = "Giant Spider"
            , cardDescription =
                "When Giant Spider is summoned, it puts a 2/11 Forest Spider into each of its empty neighboring slots. "
            , cardImage = "GiantSpiderBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 24 }
            }
        , Card
            { cardName = "Troll"
            , cardDescription =
                "At the beginning of its owner's turn, Troll heals 4 life to itself. "
            , cardImage = "TrollBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 26 }
            }
        , Card
            { cardName = "Stone Rain"
            , cardDescription =
                "Earth spell, cost 9 Stone Rain deals 25 damage to each creature. "
            , cardImage = "StoneRainBig.jpg"
            , cardCost = 9
            , cardEffect = Spell
            }
        , Card
            { cardName = "Earth Elemental"
            , cardDescription =
                "Attack is equal to its owners Earth power. Earth Elemental increases the growth of its owner's Earth power by 1. "
            , cardImage = "EarthElementalBig.jpg"
            , cardCost = 10
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Nothing , creatureLife = 50 }
            }
        , Card
            { cardName = "Master Healer"
            , cardDescription =
                "At the beginning of its owner's turn, Master Healer heals 3 life to its owner and to each of its owner's creatures. "
            , cardImage = "MasterHealerBig.jpg"
            , cardCost = 11
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 34 }
            }
        , Card
            { cardName = "Hydra"
            , cardDescription =
                "Hydra's attack damages the opponent and each of the opponent's creatures. At the beginning of its owner's turn, Hydra heals 4 life to itself. "
            , cardImage = "HydraBig.jpg"
            , cardCost = 12
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 40 }
            }
        ]
      )
    , ( "FIRE CARDS"
      , [ Card
            { cardName = "Goblin Berserker"
            , cardDescription =
                "At the beginning of its owners turn, Goblin Berserker deals 2 damage to its neighboring creatures. "
            , cardImage = "GoblinBerserkerBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 16 }
            }
        , Card
            { cardName = "Wall of Fire"
            , cardDescription =
                "When Wall of Fire is summoned it deals 5 damage to each of the opponent's creatures. "
            , cardImage = "WallofFireBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 0 , creatureLife = 5 }
            }
        , Card
            { cardName = "Priest of Fire"
            , cardDescription =
                "Priest of Fire increases the growth of its owner's Fire power by 1. "
            , cardImage = "PriestofFireBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 13 }
            }
        , Card
            { cardName = "Fire Drake"
            , cardDescription = "Fire Drake attacks the turn it enters play. "
            , cardImage = "FireDrakeBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 18 }
            }
        , Card
            { cardName = "Orc Chieftain"
            , cardDescription =
                "Orc Chieftan increases the attack of its neighboring creatures by 2 (except walls). "
            , cardImage = "OrcChieftainBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 17 }
            }
        , Card
            { cardName = "Flame Wave"
            , cardDescription =
                "Fire spell, cost 6 Flame Wave deals 9 damage to each of the opponent's creatures. "
            , cardImage = "FlameWaveBig.jpg"
            , cardCost = 6
            , cardEffect = Spell
            }
        , Card
            { cardName = "Minotaur Commander"
            , cardDescription =
                "Minotaur Commander increases by 1 the attack of all other owner's creatures (except walls). "
            , cardImage = "MinotaurCommanderBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 20 }
            }
        , Card
            { cardName = "Bargul"
            , cardDescription =
                "When Bargul is summoned it deals 4 damage to each other creature. "
            , cardImage = "BargulBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 26 }
            }
        , Card
            { cardName = "Inferno"
            , cardDescription =
                "Inferno deals 18 damage to target opponent's creature and 10 damage to each of the opponent's other creatures. "
            , cardImage = "InfernoBig.jpg"
            , cardCost = 9
            , cardEffect = Spell
            }
        , Card
            { cardName = "Fire Elemental"
            , cardDescription =
                "Attack is equal to its owner's fire power. When Fire Elemental is summoned it deals 3 damage to the opponent and to each of the opponent's creatures. Fire Elemental increases the growth of its owner's Fire power by 1. "
            , cardImage = "FireElementalBig.jpg"
            , cardCost = 10
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Nothing , creatureLife = 37 }
            }
        , Card
            { cardName = "Armageddon"
            , cardDescription =
                "Armageddon deals (8 + its caster's Fire power) damage to the opponent and to each creature. "
            , cardImage = "ArmageddonBig.jpg"
            , cardCost = 11
            , cardEffect = Spell
            }
        , Card
            { cardName = "Dragon"
            , cardDescription =
                "Dragon increases the damage its owner's spells deal by 50% (total damage will be rounded up). "
            , cardImage = "DragonBig.jpg"
            , cardCost = 12
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 9 , creatureLife = 40 }
            }
        ]
      )
    , ( "FOREST CARDS"
      , [ Card
            { cardName = "Crazy Squirrel"
            , cardDescription =
                "When Crazy Squirrel is summoned it deals 8 damage to the creature in the opposing slot. "
            , cardImage = "CrazySquirrelBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 2 , creatureLife = 2 }
            }
        , Card
            { cardName = "Forest Wolf"
            , cardDescription =
                "Magic Rabbit. Magic Rabbit is killed. The attack of Forest Wolf is equal to the attack of the killed Rabbit. "
            , cardImage = "ForestWolfBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 0 , creatureLife = 24 }
            }
        , Card
            { cardName = "Vindictive Raccoon"
            , cardDescription =
                "When Vindictive Raccoon is summoned it deals damage to the opponent equal to the attack of the creature in the opposing slot. "
            , cardImage = "VindictiveRaccoonBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 14 }
            }
        , Card
            { cardName = "Enraged Beaver"
            , cardDescription =
                "When Enraged Beaver is summoned it deals X damage to the opponent and to each of the opponent's creatures where X is equal to the attack of Magic Rabbit. "
            , cardImage = "EnragedBeaverBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 10 }
            }
        , Card
            { cardName = "Ritual of the Forest"
            , cardDescription =
                "Ritual of the Forest heals X+5 life to its caster and to each of its caster's creatures where X is equal to the attack of Magic Rabbit. "
            , cardImage = "RitualoftheForestBig.jpg"
            , cardCost = 5
            , cardEffect = Spell
            }
        , Card
            { cardName = "Treefolk Protector"
            , cardDescription =
                "Treefolk Protector decreases by 3 damage dealt by the opponent's spells. "
            , cardImage = "TreefolkProtectorBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 25 }
            }
        , Card
            { cardName = "Bee Queen"
            , cardDescription =
                "When Bee Queen is summoned it puts a 2/8 Bee Soldier into each of its empty neighbouring slots. When Bee Queen or a Bee Soldier dies it deals 3 damage to the opponent. "
            , cardImage = "BeeQueenBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 14 }
            }
        , Card
            { cardName = "Angry Angry Bear"
            , cardDescription =
                "Each time Angry Angry Bear receives damage its attack increases by 1 permanently. "
            , cardImage = "AngryAngryBearBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 30 }
            }
        ]
      )
    , ( "GOBLIN'S CARDS"
      , [ Card
            { cardName = "Rescue operation"
            , cardDescription =
                "Move target creature to a random empty slot. If that creature is owned by this spell's caster, Rescue Operation heals 5 life to it. "
            , cardImage = "RescueoperationBig.jpg"
            , cardCost = 0
            , cardEffect = Spell
            }
        , Card
            { cardName = "Goblin Hero"
            , cardDescription =
                "Goblin Hero's attack is increased by 2 for each neighbouring creature. Each time the opponent summons a creature in the opposing slot, Goblin Hero moves to a random empty slot. "
            , cardImage = "GoblinHeroBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 14 }
            }
        , Card
            { cardName = "Goblin Saboteur"
            , cardDescription =
                "Each time Goblin Saboteur deals damage to the opponent, the opponent loses the cheapest card of a random power type. "
            , cardImage = "GoblinSaboteurBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 20 }
            }
        , Card
            { cardName = "Army of Rats"
            , cardDescription =
                "Army of Rats deals 12 damage to each of the opponent's creatures, then deals 12 damage to a random caster's creature. "
            , cardImage = "ArmyofRatsBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Portal Jumper"
            , cardDescription =
                "At the end of its owner's turn Portal Jumper stuns the creature in the opposing slot, preventing it from attacking for one turn, then moves to a random slot. "
            , cardImage = "PortalJumperBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 28 }
            }
        , Card
            { cardName = "Goblin Looter"
            , cardDescription =
                "Each time any creature dies Goblin Looter increases a random power of its owner by 1. "
            , cardImage = "GoblinLooterBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 28 }
            }
        , Card
            { cardName = "Goblin Raider"
            , cardDescription =
                "When Goblin Raider is summoned, it puts two other Goblin Raiders into random empty slots. "
            , cardImage = "GoblinRaiderBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 24 }
            }
        , Card
            { cardName = "Ratmaster"
            , cardDescription =
                "At the beginning of its owner's turn Ratmaster deals 6 damage to each of the opponent's creatures and decreases a random owner's power by 3. "
            , cardImage = "RatmasterBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 47 }
            }
        ]
      )
    , ( "GOLEM CARDS"
      , [ Card
            { cardName = "Golem's Frenzy"
            , cardDescription =
                "Golem's Frenzy deals 3 damage to each of the opponent's creatures. For each creature killed by this spell, Golem's attack is increased by 3 until end of turn. "
            , cardImage = "GolemsFrenzyBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Guardian Statue"
            , cardDescription =
                "Guardian Statue receives no damage until it performs its first attack. "
            , cardImage = "GuardianStatueBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 10 }
            }
        , Card
            { cardName = "Golem's Justice"
            , cardDescription =
                "Golem's Justice deals 4 damage to each of the opponent's creatures, then heals 4 life to Golem's neighbours. "
            , cardImage = "GolemsJusticeBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Army Upgrade"
            , cardDescription =
                "Army Upgrade heals 3 life to each of its caster's creatures. Golem's attack is increased by 2 permanently; this bonus remains even after Golem is killed. "
            , cardImage = "ArmyUpgradeBig.jpg"
            , cardCost = 4
            , cardEffect = Spell
            }
        , Card
            { cardName = "Golem Instructor"
            , cardDescription =
                "Golem Instructor increases Golem's attack by 2. "
            , cardImage = "GolemInstructorBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Nothing , creatureLife = 22 }
            }
        , Card
            { cardName = "Dark Sculptor"
            , cardDescription =
                "When Dark Sculptor is summoned it deals X damage to each of the opponent's creatures, where X is equal to the number of creatures in play. "
            , cardImage = "DarkSculptorBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 2 , creatureLife = 8 }
            }
        , Card
            { cardName = "Golem Guide"
            , cardDescription =
                "When Golem Guide is summoned it swaps positions with Golem and increases Golem's attack by 3 until end of turn. "
            , cardImage = "GolemGuideBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 32 }
            }
        , Card
            { cardName = "Golem Handler"
            , cardDescription =
                "While Golem Handler is in play, Golem performs an additional attack during each of its owner's turns. "
            , cardImage = "GolemHandlerBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 40 }
            }
        ]
      )
    , ( "HOLY CARDS"
      , [ Card
            { cardName = "Paladin"
            , cardDescription =
                "When Paladin is summoned it heals 4 life to each of its owner's creatures. "
            , cardImage = "PaladinBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 9 }
            }
        , Card
            { cardName = "Monk"
            , cardDescription =
                "When Monk dies it increases its owner's Holy power by 2. "
            , cardImage = "MonkBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 13 }
            }
        , Card
            { cardName = "Holy Guard"
            , cardDescription =
                "Holy Guard decreases all damage dealt to its neighboring creatures by 2. "
            , cardImage = "HolyGuardBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 23 }
            }
        , Card
            { cardName = "Divine Justice"
            , cardDescription =
                "Divine Justice heals 12 life to target caster's creature and deals 12 damage to each other creature. "
            , cardImage = "DivineJusticeBig.jpg"
            , cardCost = 4
            , cardEffect = Spell
            }
        , Card
            { cardName = "Divine Intervention"
            , cardDescription =
                "Divine Intervention increases its caster's Fire, Water, Air and Earth powers by 2, then heals 10 life to its caster. "
            , cardImage = "DivineInterventionBig.jpg"
            , cardCost = 5
            , cardEffect = Spell
            }
        , Card
            { cardName = "Wrath of God"
            , cardDescription =
                "Wrath of God deals 12 damage to each of the opponent's creatures, then increases its caster's Holy power by 1 for each of the opponent's creatures that did not die. "
            , cardImage = "WrathofGodBig.jpg"
            , cardCost = 6
            , cardEffect = Spell
            }
        , Card
            { cardName = "Angel"
            , cardDescription =
                "When Angel is summoned it increases its owner's Holy power by 3. "
            , cardImage = "AngelBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 42 }
            }
        , Card
            { cardName = "Archangel"
            , cardDescription =
                "When Archangel is summoned it completely heals each of its owner's creatures. "
            , cardImage = "ArchangelBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 48 }
            }
        ]
      )
    , ( "ILLUSION CARDS"
      , [ Card
            { cardName = "Madness"
            , cardDescription =
                "Madness deals damage to each of the opponent's creatures equal to the attack of that creature. "
            , cardImage = "MadnessBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Phantom Warrior"
            , cardDescription =
                "Damage dealt to Phantom Warrior is reduced to 1. "
            , cardImage = "PhantomWarriorBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 4 }
            }
        , Card
            { cardName = "Hypnosis"
            , cardDescription =
                "The two opponent's creatures with the highest attack immediately attack the opponent. "
            , cardImage = "HypnosisBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Wall of Reflection"
            , cardDescription =
                "Each time Wall of Reflection receives damage it deals an equal amount of damage to the opponent, but not more than the life lost by Wall of Reflection. "
            , cardImage = "WallofReflectionBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 0 , creatureLife = 20 }
            }
        , Card
            { cardName = "Spectral Assassin"
            , cardDescription =
                "When Spectral Assassin is summoned it deals 12 damage to the opponent. "
            , cardImage = "SpectralAssassinBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 22 }
            }
        , Card
            { cardName = "Spectral Mage"
            , cardDescription =
                "When Spectral Mage is summoned it deals damage to each of the opponent's creatures equal to the cost of that creature. "
            , cardImage = "SpectralMageBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 34 }
            }
        , Card
            { cardName = "Oracle"
            , cardDescription =
                "At the beginning of its owner's turn, Oracle deals damage to the opponent equal to its owner's Illusion power. "
            , cardImage = "OracleBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 41 }
            }
        , Card
            { cardName = "Hypnotist"
            , cardDescription =
                "When Hypnotist is summoned it deals 5 damage to the opponent and to each of the opponent's creatures. Hypnotist increases the growth of its owner's Illusion power by 1. "
            , cardImage = "HypnotistBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 39 }
            }
        ]
      )
    , ( "MECHANICAL CARDS"
      , [ Card
            { cardName = "Overtime"
            , cardDescription =
                "Mechanical spell, cost 0 Overtime increases its caster's Mechanics power by 1. "
            , cardImage = "OvertimeBig.jpg"
            , cardCost = 0
            , cardEffect = Spell
            }
        , Card
            { cardName = "Dwarven Rifleman"
            , cardDescription =
                "Each time the opponent summons a creature Dwarven Rifleman deals 4 damage to that creature. "
            , cardImage = "DwarvenRiflemanBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 17 }
            }
        , Card
            { cardName = "Dwarven Craftsman"
            , cardDescription =
                "Dwarven Craftsman increases the growth of its owner's Mechanics power by 1. "
            , cardImage = "DwarvenCraftsmanBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 2 , creatureLife = 17 }
            }
        , Card
            { cardName = "Ornithopter"
            , cardDescription =
                "At the beginning of its owner's turn, Ornithopter deals 2 damage to each of the opponent's creatures. "
            , cardImage = "OrnithopterBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 24 }
            }
        , Card
            { cardName = "Steel Golem"
            , cardDescription =
                "Steel Golem receives no damage from spells and abilities. Damage dealt to Steel Golem is reduced by 1. "
            , cardImage = "SteelGolemBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 20 }
            }
        , Card
            { cardName = "Cannon"
            , cardDescription =
                "At the beginning of its owner's turn, Cannon deals 8 damage to the opponent's creature with the most life. "
            , cardImage = "CannonBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 29 }
            }
        , Card
            { cardName = "Cannonade"
            , cardDescription =
                "Cannonade deals 19 damage to each of the opponent's creatures. "
            , cardImage = "CannonadeBig.jpg"
            , cardCost = 7
            , cardEffect = Spell
            }
        , Card
            { cardName = "Steam Tank"
            , cardDescription =
                "When Steam Tank is summoned it deals 12 damage to each of the opponent's creatures. "
            , cardImage = "SteamTankBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 52 }
            }
        ]
      )
    , ( "OTHER CARDS"
      , [ Card
            { cardName = "Bee Soldier"
            , cardDescription =
                "When Bee Soldier dies it deals 3 damage to opponent. "
            , cardImage = "BeeSoldierBig.jpg"
            , cardCost = 0
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 2 , creatureLife = 8 }
            }
        , Card
            { cardName = "Initiate"
            , cardDescription =
                "While Vampire Elder is in play Initiates receive no damage. "
            , cardImage = "InitiateBig.jpg"
            , cardCost = 0
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 14 }
            }
        , Card
            { cardName = "Scrambled Lemure"
            , cardDescription = "Attack 3, life 6 "
            , cardImage = "ScrambledLemureBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 6 }
            }
        , Card
            { cardName = "Magic Rabbit"
            , cardDescription =
                "Each turn Mad Hermit summons Magic Rabbit as a passive class ability. There can be only one Magic Rabbit in play. At the beginning of its owner's turn Magic Rabbit's attack increases by 1 permanently. "
            , cardImage = "MagicRabbitBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 1 , creatureLife = 10 }
            }
        , Card
            { cardName = "Golem"
            , cardDescription =
                "Golem receives no damage from spells and abilities. When Golem dies Golem is put into another random empty slot (with full life) and its owner loses 10 life. "
            , cardImage = "GolemBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 10 }
            }
        , Card
            { cardName = "Enraged Quartermaster"
            , cardDescription = "Attack 6, life 20 "
            , cardImage = "EnragedQuartermasterBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 20 }
            }
        , Card
            { cardName = "Demon Apostate"
            , cardDescription =
                "At the beginning of its owner's turn Demon Apostate heals 2 life to its owner and to each of its owner's creatures. "
            , cardImage = "DemonApostateBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 2 , creatureLife = 25 }
            }
        , Card
            { cardName = "Rage of Souls"
            , cardDescription =
                "Deals (9 + caster's Death power) damage to each of opponent's creatures. Caster gains 2 life for each creature killed by this spell. "
            , cardImage = "RageofSoulsBig.jpg"
            , cardCost = 7
            , cardEffect = Spell
            }
        ]
      )
    , ( "SORCERY CARDS"
      , [ Card
            { cardName = "Healing Spray"
            , cardDescription =
                "Healing Spray heals 9 life to target caster's creature and 6 life to its neighboring creatures. "
            , cardImage = "HealingSprayBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Fireball"
            , cardDescription =
                "Fireball deals 9 damage to target opponent's creature and 6 damage to its neighboring creatures. "
            , cardImage = "FireballBig.jpg"
            , cardCost = 2
            , cardEffect = Spell
            }
        , Card
            { cardName = "Steal Essence"
            , cardDescription =
                "Steal Essence deals 5 damage to target creature. If the creature is killed by this spell, its caster's Sorcery power is increased by 4. "
            , cardImage = "StealEssenceBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Sacrifice"
            , cardDescription =
                "Destroy target caster's creature. Sacrifice increases its caster's Fire, Water, Air and Earth powers by 3. "
            , cardImage = "SacrificeBig.jpg"
            , cardCost = 4
            , cardEffect = Spell
            }
        , Card
            { cardName = "Ritual of Glory"
            , cardDescription =
                "Ritual of Glory completely heals each of its caster's creatures, then increases their attack by 3 until end of turn (except walls). "
            , cardImage = "RitualofGloryBig.jpg"
            , cardCost = 5
            , cardEffect = Spell
            }
        , Card
            { cardName = "Mana Burn"
            , cardDescription =
                "Mana Burn deals an amount of damage to each of the opponent's creatures equal to the opponent's highest power type, then decreases that opponent's power by 3. "
            , cardImage = "ManaBurnBig.jpg"
            , cardCost = 6
            , cardEffect = Spell
            }
        , Card
            { cardName = "Sonic Boom"
            , cardDescription =
                "Sonic Boom deals 11 damage to the opponent and to each of the opponent's creatures. Those creatures skip their attack next turn. "
            , cardImage = "SonicBoomBig.jpg"
            , cardCost = 7
            , cardEffect = Spell
            }
        , Card
            { cardName = "Disintegrate"
            , cardDescription =
                "Destroy target opponent's creature. Disintegrate deals 11 damage to each of the opponent's other creatures. "
            , cardImage = "DisintegrateBig.jpg"
            , cardCost = 8
            , cardEffect = Spell
            }
        ]
      )
    , ( "SPIRIT CARDS"
      , [ Card
            { cardName = "Crusader"
            , cardDescription =
                "When Crusader is summoned it heals 2 life to each of its owner's creatures. "
            , cardImage = "CrusaderBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 15 }
            }
        , Card
            { cardName = "Holy Avenger"
            , cardDescription =
                "Each time a neighboring creature dies, Holy Avenger's attack increases by 2 permanently. "
            , cardImage = "HolyAvengerBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 23 }
            }
        , Card
            { cardName = "Templar"
            , cardDescription =
                "Each time its owner summons a creature into a neighboring slot, Templar deals 4 damage to the opponent. "
            , cardImage = "TemplarBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 26 }
            }
        , Card
            { cardName = "Divine Justice"
            , cardDescription =
                "Divine Justice heals 12 life to target caster's creature and deals 12 damage to each other creature. "
            , cardImage = "DivineJusticeBig.jpg"
            , cardCost = 4
            , cardEffect = Spell
            }
        , Card
            { cardName = "Divine Meddling"
            , cardDescription =
                "Divine Meddling increases its caster's Fire, Water, Air and Earth powers by 2, then deals 10 damage to the opponent. "
            , cardImage = "DivineMeddlingBig.jpg"
            , cardCost = 5
            , cardEffect = Spell
            }
        , Card
            { cardName = "Rage of God"
            , cardDescription =
                "Rage of God deals 12 damage to each of the opponent's creatures, then deals 3 damage to the opponent for each of the opponent's creatures that did not die. "
            , cardImage = "RageofGodBig.jpg"
            , cardCost = 6
            , cardEffect = Spell
            }
        , Card
            { cardName = "Angel"
            , cardDescription =
                "When Angel is summoned it increases its owner's Holy power by 3. "
            , cardImage = "AngelBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 42 }
            }
        , Card
            { cardName = "Angel of War"
            , cardDescription =
                "When Angel of War is summoned it deals 8 damage to each of the opponent's creatures and heals 8 life to each of its owner's creatures. "
            , cardImage = "AngelofWarBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 37 }
            }
        ]
      )
    , ( "TIME CARDS"
      , [ Card
            { cardName = "Chrono Hunter"
            , cardDescription =
                "Each time Chrono Hunter kills an opponent's creature it increases its owner's Time power by 2. "
            , cardImage = "ChronoHunterBig.jpg"
            , cardCost = 1
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 11 }
            }
        , Card
            { cardName = "Timeblazer"
            , cardDescription =
                "When Timeblazer is summoned each of its neighbouring creatures attack the opponent this turn instead of the creature in the opposing slot. "
            , cardImage = "TimeblazerBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 17 }
            }
        , Card
            { cardName = "Hasten"
            , cardDescription =
                "Target caster's creature with attack 9 or less immediately attacks the opponent and all the opponent's creatures. "
            , cardImage = "HastenBig.jpg"
            , cardCost = 3
            , cardEffect = Spell
            }
        , Card
            { cardName = "Time Stop"
            , cardDescription =
                "Time Stop deals 4 damage to each of the opponent's creatures and forces the opponent to skip their next turn. "
            , cardImage = "TimeStopBig.jpg"
            , cardCost = 4
            , cardEffect = Spell
            }
        , Card
            { cardName = "Timeweaver"
            , cardDescription =
                "When Timeweaver is summoned its owner is able to cast an additional spell this turn. Timeweaver decreases the cost of its owner's spells by 1. "
            , cardImage = "TimeweaverBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 24 }
            }
        , Card
            { cardName = "Priestess of Moments"
            , cardDescription =
                "Priestess of Moments performs an additional attack in each of its owner's turns, and allows its owner's other creatures to attack the turn they enter play. "
            , cardImage = "PriestessofMomentsBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 35 }
            }
        , Card
            { cardName = "Chrono Engine"
            , cardDescription =
                "Chrono Engine allows its owner to use an additional card each turn. Each time its owner skips a turn Chrono Engine increases its owner's Time power by 1. "
            , cardImage = "ChronoEngineBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 33 }
            }
        , Card
            { cardName = "Time Dragon"
            , cardDescription =
                "When Time Dragon is summoned its owner may use an additional card this turn. Time Dragon attacks the turn it enters play. "
            , cardImage = "TimeDragonBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 40 }
            }
        ]
      )
    , ( "VAMPIRIC CARDS"
      , [ Card
            { cardName = "Blood Boil"
            , cardDescription =
                "Blood Boil deals 4 damage to each of the opponent's creatures, then increases its owner's Blood power by 1 for each creature killed by this spell. "
            , cardImage = "BloodBoilBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Ghoul"
            , cardDescription =
                "Each time an opponent's creature dies Ghoul's attack increases by 1 permanently. "
            , cardImage = "GhoulBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 25 }
            }
        , Card
            { cardName = "Devoted Servant"
            , cardDescription =
                "'+'At the beginning of its owner's turn, Devoted Servant's attack increases by 1 permanently. When Devoted Servant dies it increases its owner's Blood power by an amount equal to its attack. "
            , cardImage = "DevotedServantBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 1 , creatureLife = 19 }
            }
        , Card
            { cardName = "Vampire Mystic"
            , cardDescription =
                "Each time the opponent receives damage, Vampire Mystic's attack increases by 2 until end of turn. "
            , cardImage = "VampireMysticBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 46 }
            }
        , Card
            { cardName = "Justicar"
            , cardDescription =
                "The opposing creature receives double damage from all sources. While Justicar is unblocked all damage done to the opponent is increased by 2. "
            , cardImage = "JusticarBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 49 }
            }
        , Card
            { cardName = "Chastiser"
            , cardDescription =
                "Each time its owner receives damage Chastiser's attack increases by 2 until after its next attack. "
            , cardImage = "ChastiserBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 9 , creatureLife = 51 }
            }
        , Card
            { cardName = "Vampire Elder"
            , cardDescription =
                "When Vampire Elder is summoned it puts a 5/14 Initiate into each of its empty neighboring slots. While Vampire Elder is in play Initiates receive no damage. "
            , cardImage = "VampireElderBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 27 }
            }
        , Card
            { cardName = "Magister of Blood"
            , cardDescription =
                "When Magister of Blood is summoned it deals 16 damage to the opponent and to each of the opponent's blocked creatures. "
            , cardImage = "MagisterofBloodBig.jpg"
            , cardCost = 8
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 8 , creatureLife = 33 }
            }
        ]
      )
    , ( "WATER CARDS"
      , [ Card
            { cardName = "Meditation"
            , cardDescription =
                "Meditation increases its caster's Fire, Air and Earth powers by 1. "
            , cardImage = "MeditationBig.jpg"
            , cardCost = 1
            , cardEffect = Spell
            }
        , Card
            { cardName = "Sea Sprite"
            , cardDescription =
                "At the beginning of its owner's turn, Sea Sprite deals 2 damage to its owner. "
            , cardImage = "SeaSpriteBig.jpg"
            , cardCost = 2
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 22 }
            }
        , Card
            { cardName = "Merfolk Apostate"
            , cardDescription =
                "When Merfolk Apostate is summoned it increases its owner's Fire power by 2. "
            , cardImage = "MerfolkApostateBig.jpg"
            , cardCost = 3
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 10 }
            }
        , Card
            { cardName = "Ice Golem"
            , cardDescription =
                "Ice Golem receives no damage from spells and abilites. "
            , cardImage = "IceGolemBig.jpg"
            , cardCost = 4
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 4 , creatureLife = 12 }
            }
        , Card
            { cardName = "Merfolk Elder"
            , cardDescription =
                "Merfolk Elder increases the growth of its owner's Air power by 1. "
            , cardImage = "MerfolkElderBig.jpg"
            , cardCost = 5
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 16 }
            }
        , Card
            { cardName = "Ice Guard"
            , cardDescription =
                "Ice Guard reduces all damage dealt to its owner by 50% (total damage will be rounded up). "
            , cardImage = "IceGuardBig.jpg"
            , cardCost = 6
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 3 , creatureLife = 20 }
            }
        , Card
            { cardName = "Giant Turtle"
            , cardDescription =
                "Damage dealt to Giant Turtle is reduced by 5. "
            , cardImage = "GiantTurtleBig.jpg"
            , cardCost = 7
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 5 , creatureLife = 16 }
            }
        , Card
            { cardName = "Acidic Rain"
            , cardDescription =
                "Acidic Rain deals 15 damage to each creature, then decreases each of the opponent's powers by 1. "
            , cardImage = "AcidicRainBig.jpg"
            , cardCost = 8
            , cardEffect = Spell
            }
        , Card
            { cardName = "Merfolk Overlord"
            , cardDescription =
                "Merfolk Overlord allows creatures in its neighboring slots to attack the turn they enter play. "
            , cardImage = "MerfolkOverlordBig.jpg"
            , cardCost = 9
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 7 , creatureLife = 35 }
            }
        , Card
            { cardName = "Water Elemental"
            , cardDescription =
                "Attack is equal to its owner's water power.  When Water Elemental is summoned it heals 10 life to its owner. Water Elemental increases the growth of its owner's Water power by 1. "
            , cardImage = "WaterElementalBig.jpg"
            , cardCost = 10
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Nothing , creatureLife = 37 }
            }
        , Card
            { cardName = "Mind Master"
            , cardDescription =
                "Mind Master increases the growth of each of its owner's powers by 1. "
            , cardImage = "MindMasterBig.jpg"
            , cardCost = 11
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 6 , creatureLife = 23 }
            }
        , Card
            { cardName = "Astral Guard"
            , cardDescription =
                "Astral Guard decreases the growth of each of the opponent's powers by 1. "
            , cardImage = "AstralGuardBig.jpg"
            , cardCost = 12
            , cardEffect =
                Creature
                  CreatureCard { creatureAttack = Just 1 , creatureLife = 18 }
            }
        ]
      )
    ]
