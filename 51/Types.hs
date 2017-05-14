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
  , locationBuildBonus  :: Maybe Description
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

data Description =
    OneTimeCache [(Int,Resource)]
  | ForEachLoc LocType Int Resource Int
    -- ^ For each location of the given type, gain the given number of
    -- resources, up to the limit
  | ForEachProduced Resource Int Resource Int
    -- ^ For each produced resource of the given type, gain the given number of
    -- resources, up to the limit

  | IfProducedAtLeast Int Resource' Int Resource
    -- If you produced the required number of the first thing,
    -- get the given number of the seconf thing.
  | Storage [ (Maybe Int,Resource') ]
  | Other [Lexeme]
  deriving Show

data Resource' = AnyBasicResource | ExactResource Resource
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


