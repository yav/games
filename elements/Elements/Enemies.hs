{-# LANGUAGE Safe, OverloadedStrings #-}
module Elements.Enemies where

import Util.ResourceQ(ResourceQ)
import Util.JSON

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import           Data.Text (Text)

type EnemyPool = Map EnemyType (ResourceQ Enemy)

data EnemyAbility =
    Sturdy    -- ^ Attacks half efficient
  | Quick     -- ^ Needs twice the usual block
  | Strong    -- ^ Deals twice the usual damage
  | Poisons   -- ^ Get twice the wounds. Hero: half in discard pile.
  | Stuns     -- ^ Hero: discard non-wound cards;  Unit: destroyed.
    deriving (Show,Eq,Ord)

-- XXX: too many enemy types;  we could have just a few "flavors"
-- and generate random enemies matching the flavor.
data EnemyType  = Orc | Guardian | Mage | Underworld | Citizen | Draconum
                  deriving (Eq,Ord,Show,Enum,Bounded)

allEnemyTypes :: [EnemyType]
allEnemyTypes = [ minBound .. maxBound ]

data Enemy = Enemy
  { enemyName       :: Text
  , enemyType       :: EnemyType          -- ^ The color of the token
  , enemyArmor      :: Int                -- ^ How much armor
  , enemyAttack     :: Int
  , enemyFameGain   :: Int                -- ^ Fame gain for win
  , enemyAbilities  :: Set EnemyAbility   -- ^ special abilities
  } deriving Show

instance Eq Enemy where
  x == y = enemyName x == enemyName y

instance Ord Enemy where
  compare x y = compare (enemyName x) (enemyName y)


enemyTypeText :: EnemyType -> Text
enemyTypeText et =
  case et of
    Orc         -> "orc"
    Guardian    -> "guardian"
    Mage        -> "mage"
    Underworld  -> "underworld"
    Citizen     -> "citizen"
    Draconum    -> "draconum"

instance Export EnemyType where
  toJS = toJS . enemyTypeText



--------------------------------------------------------------------------------
-- XXX: Make a random generator

allEnemies :: [Enemy]
allEnemies = orcs ++ keep ++ dungeon ++ magical ++ draconum ++ citizens


orcs :: [Enemy]
orcs = concat

  [ replicate 2 Enemy
      { enemyName      = "Prowlers"
      , enemyType      = Orc
      , enemyArmor     = 3
      , enemyAttack    = 4
      , enemyFameGain  = 2
      , enemyAbilities = Set.empty
      }

  , replicate 2 Enemy
      { enemyName      = "Diggers"
      , enemyType      = Orc
      , enemyArmor     = 3
      , enemyAbilities = Set.fromList [ Sturdy ]
      , enemyAttack    = 3
      , enemyFameGain  = 2
      }

  , replicate 2 Enemy
      { enemyName      = "Cursed Hags"
      , enemyType      = Orc
      , enemyArmor     = 5
      , enemyAttack    = 3
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Wolf Riders"
      , enemyType      = Orc
      , enemyArmor     = 4
      , enemyAttack    = 3
      , enemyAbilities = Set.fromList [ Quick ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Ironclads"
      , enemyType      = Orc
      , enemyArmor     = 3
      , enemyAttack    = 4
      , enemyAbilities = Set.fromList [ Strong ]
      , enemyFameGain  = 4
      }

{-
  , replicate 2 Enemy
      { enemyName      = "Orc Summoners"
      , enemyType      = Orc
      , enemyArmor     = 4
      , enemyAttack    = Summoner
      , enemyFameGain  = 4
      , enemyAbilities = Set.empty
      }
-}
  ]


keep :: [Enemy]
keep = concat

  [ replicate 3 Enemy
      { enemyName      = "Crossbowmen"
      , enemyType      = Guardian
      , enemyArmor     = 4
      , enemyAttack    = 4
      , enemyAbilities = Set.fromList [ Quick ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Golems"
      , enemyType      = Guardian
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Sturdy ]
      , enemyAttack    = 2
      , enemyFameGain  = 4
      }

  , replicate 3 Enemy
      { enemyName      = "Guardsmen"
      , enemyType      = Guardian
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Sturdy ]
      , enemyAttack    = 3
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Swordsmen"
      , enemyType      = Guardian
      , enemyArmor     = 5
      , enemyAbilities = Set.empty
      , enemyAttack    = 6
      , enemyFameGain  = 4
      }
  ]


dungeon :: [Enemy]
dungeon = concat

  [ replicate 2 Enemy
      { enemyName      = "Crypt Worm"
      , enemyType      = Underworld
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Sturdy ]
      , enemyAttack    = 6
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Gargoyle"
      , enemyType      = Underworld
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Sturdy ]
      , enemyAttack    = 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Medusa"
      , enemyType      = Underworld
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Stuns ]
      , enemyAttack    = 6
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Minotaur"
      , enemyType      = Underworld
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Strong ]
      , enemyAttack    = 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Werewolf"
      , enemyType      = Underworld
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Quick ]
      , enemyAttack    = 7
      , enemyFameGain  = 5
      }
  ]



magical :: [Enemy]
magical = concat
  [ replicate 2 Enemy
      { enemyName      = "Monks"
      , enemyType      = Mage
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyAttack    = 5
      , enemyFameGain  = 4
      }

{-
  , replicate 2 Enemy
      { enemyName      = "Illusionists"
      , enemyType      = Mage
      , enemyArmor     = 3
      , enemyAbilities = Set.fromList [ Sturdy ]
      , enemyAttack    = Summoner
      , enemyFameGain  = 4
      } -}

  , replicate 2 Enemy
      { enemyName      = "Ice Mages"
      , enemyType      = Mage
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ ] -- Ice
      , enemyAttack    = 5 -- Ice
      , enemyFameGain  = 5
      }

  , replicate 1 Enemy
      { enemyName      = "Ice Golems"
      , enemyType      = Mage
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Sturdy, Stuns ] -- Ice
      , enemyAttack    = 2 -- Ice
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Fire Mages"
      , enemyType      = Mage
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ ] -- Fire
      , enemyAttack    = 6 -- Fire
      , enemyFameGain  = 5
      }

  , replicate 1 Enemy
      { enemyName      = "Fire Golems"
      , enemyType      = Mage
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Sturdy, Strong ] -- Fire
      , enemyAttack    = 3 -- Fire
      , enemyFameGain  = 5
      }

  ]


draconum :: [Enemy]
draconum = concat
  [ replicate 2 Enemy
      { enemyName      = "Fire Dragon"
      , enemyType      = Draconum
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Sturdy ] -- Fire
      , enemyAttack    = 9 -- Fire
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "High Dragon"
      , enemyType      = Draconum
      , enemyArmor    = 9
      , enemyAbilities = Set.fromList [ Strong ] -- Fire, Ice
      , enemyAttack    = 6 -- ColdFire
      , enemyFameGain  = 9
      }

  , replicate 2 Enemy
      { enemyName      = "Ice Dragon"
      , enemyType      = Draconum
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Sturdy, Stuns] -- Ice
      , enemyAttack    = 6 -- Ice
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "Swamp Dragon"
      , enemyType      = Draconum
      , enemyArmor    = 9
      , enemyAbilities = Set.fromList [ Quick, Poisons ]
      , enemyAttack    = 5
      , enemyFameGain  = 7
      }
  ]


citizens :: [Enemy]
citizens = concat
  [ replicate 2 Enemy
      { enemyName      = "Altem Guardsmen"
      , enemyType      = Citizen
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Sturdy ] -- Fire, Ice
      , enemyAttack    = 6
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "Altem Mages"
      , enemyType      = Citizen
      , enemyArmor    = 8
      , enemyAbilities = Set.fromList [ Sturdy, Strong, Poisons ]
      , enemyAttack    = 4 -- ColdFire
      , enemyFameGain  = 8
      }

  , replicate 3 Enemy
      { enemyName      = "Freezers"
      , enemyType      = Citizen
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Quick, Stuns ] -- Fire
      , enemyAttack    = 3 -- Ice
      , enemyFameGain  = 7
      }

  , replicate 3 Enemy
      { enemyName      = "Gunners"
      , enemyType      = Citizen
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Strong ] -- Fire
      , enemyAttack    = 6 -- Fire
      , enemyFameGain  = 7
      }

  ]
