module Types where

import Data.Text(Text)
import Data.Set(Set)
import Util.Bag(Bag)

data Connection = Connection
  { connectionName    :: Text
  , connectionText    :: Description
  , connectionSet     :: LocationSet
  } deriving (Show)

data Location = Location
  { locationName        :: Text
  , locationClass       :: LocationClass
  , locationDistance    :: Int
  , locationSpoils      :: Bag Resource
  , locationType        :: Set LocType
  , locationText        :: Description
  , locationBuildBonus  :: Description
  , locationDeal        :: Resource
  , locationSet         :: LocationSet
  } deriving (Show)

data LocationSet = BaseSet | Winter | NewEra | PromoSet1
  deriving (Show,Eq,Ord,Enum,Bounded)

data LocationClass = Action | Feature | Production ProductionType
  deriving (Show,Eq,Ord)

data ProductionType = Closed | Open
  deriving (Show,Eq,Ord)

data LocType = LocBrick | LocWorker | LocVictory
             | LocGun | LocAmmo | LocIron | LocFuel | LocContact
             | LocToxic | LocLiberty | LocCard
  deriving (Show,Eq,Ord,Enum,Bounded)

data Resource = Brick | Worker | VP | Gun | Ammo | Iron | Fuel
              | ContactRed | ContactBlue | ContactGrey | ContactUniversal
              | NewCard | Deal | Shield | Development
  deriving (Show,Eq,Ord,Enum,Bounded)

data Limit = NoLimit | Limit Int
  deriving (Show,Eq)

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

  | None

  deriving Show

data Event = MadeADeal
           | RazedSomething
           | Produced Int Resource'
           | Either Event Event
  deriving Show

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



