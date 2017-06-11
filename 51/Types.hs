module Types where

import Data.Text(Text)
import Data.Set(Set)
import Util.Bag(Bag)

-- | Connection cards
data Connection = Connection
  { connectionName    :: Text           -- ^ Identifies a connection
  , connectionText    :: Description    -- ^ What does it do
  , connectionSet     :: LocationSet    -- ^ Where did it come from
  } deriving (Show)

-- | All other cards
data Location = Location
  { locationName        :: Text           -- ^ Identifies a location
  , locationClass       :: LocationClass  -- ^ Classifies the location
  , locationDistance    :: Int            -- ^ Cost to use from hand
  , locationSpoils      :: Bag Resource   -- ^ Gains if raized from hand
  , locationType        :: Set LocType    -- ^ Location type (e.g., for upgrade)
  , locationText        :: Description    -- ^ What does it do
  , locationBuildBonus  :: Description    -- ^ Happens once, when built
  , locationDeal        :: Resource       -- ^ If used as a deal
  , locationSet         :: LocationSet    -- ^ Where did it come from
  } deriving (Show)

-- | Sets of cards
data LocationSet = BaseSet | Winter | NewEra | PromoSet1
  deriving (Show,Eq,Ord,Enum,Bounded)

-- | Different location classes
data LocationClass = Action | Feature | Production ProductionType
  deriving (Show,Eq,Ord)

-- | Type of production
data ProductionType = Closed | Open
  deriving (Show,Eq,Ord)

-- | Location types for upgrade etc
data LocType = LocBrick | LocWorker | LocVictory
             | LocGun | LocAmmo | LocIron | LocFuel | LocContact
             | LocToxic | LocLiberty | LocCard
  deriving (Show,Eq,Ord,Enum,Bounded)

-- | Resources.  NewCard and Deal perhaps should not be here
data Resource = Brick | Worker | VP | Gun | Ammo | Iron | Fuel
              | ContactRed | ContactBlue | ContactGrey | ContactUniversal
              | NewCard | Deal | Shield | Development
  deriving (Show,Eq,Ord,Enum,Bounded)

-- | Used to limit thing, or not.
data Limit = NoLimit | Limit Int
  deriving (Show,Eq)

-- | Various things done by locations.
data Description =
    OneTimeCache [(Int,Resource)]
    -- ^ This is a location bonus.
    -- Place the given amounts of resources on the card, they can be
    -- used during a turn, and do not need to be discarded at the end.

  | ForEachLoc LocType Int Resource Limit
    -- ^ For each location of the given type, gain the given number of
    -- resources, up to the limit
  | ForEachProduced Resource Int Resource Limit
    -- ^ For each produced resource of the given type, gain the given number of
    -- resources, up to the limit

  | EachTime Event Int Resource

  | Storage [ (Limit,Resource') ]
    -- ^ You may store the given resource, so they persist across turns.

  | Do Action

  | Act [ (Int,Resource') ] Action Limit
    -- ^ Pay the cost to do the actions.
    -- The limit indicates how many times can you use this.

  | SecretStock
    -- ^ each time you spend 2 workers to gain a resource of the type
    -- produced by your faction board gain two of them instead

  | DecreaseDefence Int
    -- ^ Decrease the defence of foreign locations when we attack them

  | None
    -- ^ Does nothing

  deriving Show

-- | Various events used in cards
data Event = MadeADeal
           | RazedSomething
           | Produced Int Resource'
           | Either Event Event
  deriving Show

-- | Something that can be done.
data Action = Gain Int Resource
            | GainBasic Int
            | StealBasic Int
            | ResetLocations Int
            | UseDeal Owner Int
            | ActivateProduction Owner Int
            | UseBlockedOpenProduction
            | Draw2Keep1
            | Choose Action Action
            | Action `AndAlso` Action
  deriving Show

data Owner = Yours | Others
  deriving Show

data Resource' = A Resource
               | AnyBasicResource
  deriving (Eq,Show)

isBasicResource :: Resource -> Bool
isBasicResource x = x == Brick || x == Gun || x == Ammo || x == Fuel



