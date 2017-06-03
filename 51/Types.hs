module Types where

data Connection = Connection
  { connectionName    :: String
  , connectionText    :: Description
  , connectionSet     :: LocationSet
  } deriving (Show)

data Location = Location
  { locationName        :: String
  , locationClass       :: LocationClass
  , locationDistance    :: Int
  , locationSpoils      :: [(Int,Resource)]
  , locationType        :: [LocType]
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
              | NewCard | Shield | Development
  deriving (Show,Eq,Ord,Enum,Bounded)

data Limit = NoLimit | Limit Int
  deriving (Show)

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

  | IfProducedAtLeast Int Resource' Int Resource
    -- If you produced the required number of the first thing,
    -- get the given number of the seconf thing.

  | Storage [ (Limit,Resource') ]
    -- ^ You may store the given resource, so they persist across turns.

  | Convert [ (Int,Resource') ] [ (Int,Resource) ] Int
    -- ^ Spend the first resource to gain the second
    -- The last 'Int' is the number of activations.

  | None


  | Other [Lexeme]
  deriving Show


data Resource' = AnyOf [Resource]
  deriving Show


isBasicResource :: Resource -> Bool
isBasicResource x = x == Brick || x == Gun || x == Ammo || x == Fuel

data Lexeme  = LexR2 Int What
             | LexL2 LocType
             | LexW2 String
             | ActivatedTwice
               deriving Show

data What = YourLocation | BasicResource | Resource Resource
          | YourDeals
          | AnothersDeals
               deriving Show


