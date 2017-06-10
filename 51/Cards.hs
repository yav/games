{-# Language OverloadedStrings #-}
module Cards where

import Util.Bag
import qualified Data.Set as Set
import Types

connections :: [(Int,Connection)]
connections =
  [ ( 2
    , Connection
        { connectionName = "Junk Train"
        , connectionText = Do (Gain 3 ContactBlue)
        , connectionSet = BaseSet
        }
    )
  , ( 4
    , Connection
        { connectionName = "Merchants"
        , connectionText = Do (Gain 2 ContactBlue)
        , connectionSet = BaseSet
        }
    )
  , ( 4
    , Connection
        { connectionName = "Punks"
        , connectionText = Do (Gain 2 ContactRed)
        , connectionSet = BaseSet
        }
    )
  , ( 2
    , Connection
        { connectionName = "Thugs"
        , connectionText =
            Act [ ( 1 , A Gun ) ] (Gain 3 ContactRed) (Limit 1)
        , connectionSet = BaseSet
        }
    )
  ]


locations :: [(Int,Location)]
locations =
  [ ( 1
    , Location
        { locationName = "Abandoned Suburbs"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Brick ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocBrick ]
        , locationText = Act [ ( 2 , A Brick ) ] (Gain 2 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Archive"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocWorker , LocVictory ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Arena"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Gun ) ]
        , locationType = Set.fromList [ LocGun , LocAmmo ]
        , locationText = ForEachLoc LocGun 1 Gun (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Armed Merchants"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocVictory , LocAmmo ]
        , locationText = EachTime (Produced 5 AnyBasicResource) 2 VP
        , locationBuildBonus = None
        , locationDeal = VP
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Assassin"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) ]
        , locationType = Set.fromList [ LocGun , LocAmmo ]
        , locationText = Do (Gain 1 Gun)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Assembly Plant"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Iron ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocIron , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 2 , A Iron ) ] (Gain 3 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Bioweaponry"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 1 , A Gun ) ] (Gain 2 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Black Fortress"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Iron ) , ( 1 , Gun ) ]
        , locationType = Set.fromList [ LocIron , LocGun ]
        , locationText = Do (Gain 1 Iron `AndAlso` Gain 1 Gun)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Black Market"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) ]
        , locationType = Set.fromList [ LocBrick , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker )
              , ( 1 , A Brick )
              , ( 1 , A Iron )
              , ( 1 , A Gun )
              ]
              (Gain 3 VP)
              (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Black market Contacts"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactBlue ) ]
        , locationType = Set.fromList [ LocContact , LocFuel ]
        , locationText = Storage [ ( Limit 3 , A ContactBlue ) ]
        , locationBuildBonus = Do (Gain 1 ContactBlue)
        , locationDeal = ContactBlue
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Body Hunters"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Worker ) ]
        , locationType = Set.fromList [ LocWorker , LocToxic ]
        , locationText =
            Act [ ( 1 , A Worker ) ] (ResetLocations 3) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Boiler Room"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Worker ) ]
        , locationType = Set.fromList [ LocWorker ]
        , locationText = Do (Gain 2 Worker)
        , locationBuildBonus = Do (Gain 1 Worker)
        , locationDeal = Worker
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Boiler Room"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Worker ) ]
        , locationType = Set.fromList [ LocWorker ]
        , locationText = Do (Gain 2 Worker)
        , locationBuildBonus = Do (Gain 1 Worker)
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Brick Storage"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) ]
        , locationType = Set.fromList [ LocBrick ]
        , locationText = None
        , locationBuildBonus = OneTimeCache [ ( 3 , Brick ) ]
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Brick Supplier"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Brick ) ]
        , locationType = Set.fromList [ LocBrick , LocAmmo ]
        , locationText = ForEachLoc LocBrick 1 Brick (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Brick Village"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) ]
        , locationType = Set.fromList [ LocBrick ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 2 Brick) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Builders"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , Brick ) ]
        , locationType = Set.fromList [ LocBrick , LocWorker ]
        , locationText = Act [ ( 1 , A Brick ) ] (Gain 1 Worker) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Bus Station"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocWorker , LocFuel ]
        , locationText = EachTime MadeADeal 1 Worker
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Camino Real"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) , ( 1 , Worker ) ]
        , locationType = Set.fromList [ LocGun , LocContact ]
        , locationText =
            Act [ ( 1 , A Worker ) ] (UseDeal Others 1) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Camp"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocWorker ]
        , locationText = None
        , locationBuildBonus = OneTimeCache [ ( 3 , Worker ) ]
        , locationDeal = Worker
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Car Garage"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped
            [ ( 1 , ContactRed ) , ( 1 , Gun ) , ( 1 , Worker ) ]
        , locationType = Set.fromList [ LocToxic , LocGun ]
        , locationText = Do (Gain 1 Shield)
        , locationBuildBonus = Do (Gain 1 Shield)
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Caravanner"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) ]
        , locationType = Set.fromList [ LocFuel , LocToxic ]
        , locationText =
            Act [ ( 1 , A Worker ) ] (UseDeal Yours 3) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Church"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Do (Gain 2 VP)
        , locationDeal = VP
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Church"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Do (Gain 2 VP)
        , locationDeal = VP
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Cistern"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText = ForEachProduced Fuel 1 VP (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "City Guards"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) ]
        , locationType = Set.fromList [ LocGun ]
        , locationText = None
        , locationBuildBonus = OneTimeCache [ ( 3 , Gun ) ]
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Clay Pit"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocBrick , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 1 , A Brick ) ] (Gain 2 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Combat Vehicle"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 3 , Shield ) ]
        , locationType = Set.fromList [ LocGun , LocContact ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 1 Shield) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Shield
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Combat Zone"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) ]
        , locationType = Set.fromList [ LocGun ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 2 Gun) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Confessor"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Gun ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocGun ]
        , locationText = Act [ ( 2 , A Gun ) ] (Gain 2 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Construction site"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocBrick , LocVictory ]
        , locationText = ForEachProduced Fuel 1 VP (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = Winter
        }
    )
  , ( 3
    , Location
        { locationName = "Construction Vehicles"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactGrey ) ]
        , locationType = Set.fromList [ LocIron , LocContact ]
        , locationText = Do (Gain 1 ContactGrey)
        , locationBuildBonus = Do (Gain 1 ContactGrey)
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Convoy"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactBlue ) ]
        , locationType = Set.fromList [ LocFuel , LocContact ]
        , locationText =
            Act [ ( 1 , A Fuel ) ] (Gain 1 ContactBlue) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = ContactBlue
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Corner Shop"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) ]
        , locationType = Set.fromList [ LocAmmo , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 1 , AnyBasicResource ) ]
              (Gain 1 VP)
              (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Court House"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocBrick , LocAmmo ]
        , locationText = Do (Gain 1 Brick `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Court House"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocBrick , LocAmmo ]
        , locationText = Do (Gain 1 Brick `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Craftsmen Guild"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Iron ) , ( 1 , Brick ) ]
        , locationType = Set.fromList [ LocIron , LocBrick ]
        , locationText = Do (Choose (Gain 1 Brick) (Gain 1 Iron))
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Crossing"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , ContactGrey ) ]
        , locationType = Set.fromList [ LocContact ]
        , locationText = Storage [ ( Limit 3 , A ContactGrey ) ]
        , locationBuildBonus = Do (Gain 4 ContactGrey)
        , locationDeal = ContactGrey
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Crossroads"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , NewCard ) ]
        , locationType = Set.fromList [ LocWorker , LocCard ]
        , locationText = None
        , locationBuildBonus = Do (Gain 2 NewCard)
        , locationDeal = NewCard
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Dark Visions Gang"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactRed ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocGun , LocContact ]
        , locationText = EachTime (Produced 1 (A Gun)) 1 ContactRed
        , locationBuildBonus = None
        , locationDeal = ContactRed
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Demolition Team"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 1 , Development ) , ( 1 , Brick ) ]
        , locationType = Set.fromList [ LocBrick , LocContact ]
        , locationText = EachTime (Produced 1 (A Brick)) 1 Development
        , locationBuildBonus = None
        , locationDeal = Development
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Deserted Colony"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocAmmo ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 1 Ammo) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Disassembly Workshop"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) ]
        , locationType = Set.fromList [ LocIron ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 2 Iron) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Docks"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) ]
        , locationType = Set.fromList [ LocIron , LocAmmo ]
        , locationText = Do (Gain 1 Iron)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Dream City"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Worker ) ]
        , locationType = Set.fromList [ LocVictory , LocWorker ]
        , locationText = EachTime (Produced 2 (A VP)) 1 Worker
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Eight Mile"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker )
              , ( 1 , A Fuel )
              , ( 1 , A Brick )
              , ( 1 , A Iron )
              ]
              (Gain 3 VP)
              (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Espionage Center"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , NewCard ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocFuel , LocCard ]
        , locationText = EachTime MadeADeal 1 NewCard
        , locationBuildBonus = None
        , locationDeal = NewCard
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Excavator"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocBrick , LocContact ]
        , locationText = Do (Gain 1 Development)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Excavator"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocBrick , LocContact ]
        , locationText = Do (Gain 1 Development)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Excavator"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocBrick , LocContact ]
        , locationText = Do (Gain 1 Development)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Expedition Camp"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocGun , LocAmmo ]
        , locationText = Do (Gain 1 Gun `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Expedition Camp"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocGun , LocAmmo ]
        , locationText = Do (Gain 1 Gun `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Factory"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) , ( 1 , Iron ) ]
        , locationType = Set.fromList [ LocIron , LocVictory ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocIron 1 VP (Limit 5)
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Foundation"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactGrey ) ]
        , locationType = Set.fromList []
        , locationText = None
        , locationBuildBonus = OneTimeCache [ ( 1 , Development ) ]
        , locationDeal = Brick
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Fuel Tank"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) ]
        , locationType = Set.fromList [ LocFuel , LocAmmo ]
        , locationText = Do (Gain 1 Fuel)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Gangers' Dive"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactRed ) ]
        , locationType = Set.fromList [ LocContact , LocFuel ]
        , locationText = Storage [ ( Limit 3 , A ContactRed ) ]
        , locationBuildBonus = Do (Gain 1 ContactRed)
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Gasoline Cultist"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped
            [ ( 1 , ContactBlue ) , ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText =
            Act [ ( 1 , A ContactBlue ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Gasoline Drinkers Den"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Fuel ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocFuel ]
        , locationText = Act [ ( 2 , A Fuel ) ] (Gain 2 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Gasoline Tower"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , Fuel ) ]
        , locationType = Set.fromList [ LocWorker , LocFuel ]
        , locationText = Act [ ( 1 , A Fuel ) ] (Gain 1 Worker) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Guild's Garage"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , NewCard ) ]
        , locationType = Set.fromList [ LocFuel , LocCard ]
        , locationText = EachTime MadeADeal 1 NewCard
        , locationBuildBonus = None
        , locationDeal = NewCard
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Gun Shop"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Gun ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 2 , A Gun ) ] (Gain 3 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Gunsmith"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactRed ) ]
        , locationType = Set.fromList [ LocGun , LocContact ]
        , locationText =
            Act [ ( 1 , A Gun ) ] (Gain 1 ContactRed) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = ContactRed
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Hanger"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocFuel , LocAmmo ]
        , locationText = Do (Gain 1 Fuel `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Hanger"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocFuel , LocAmmo ]
        , locationText = Do (Gain 1 Fuel `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Haven"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocIron , LocAmmo ]
        , locationText = Do (Gain 1 Iron `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Haven"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocIron , LocAmmo ]
        , locationText = Do (Gain 1 Iron `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Hibernation Chambers"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocWorker ]
        , locationText = Storage [ ( NoLimit , A Worker ) ]
        , locationBuildBonus = Do (Gain 4 Worker)
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Hidden Forge"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocToxic , LocGun ]
        , locationText = Do (Gain 1 Shield)
        , locationBuildBonus = None
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Hideout"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText = Act [ ( 1 , A Deal ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = VP
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Huge Machinery"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactGrey ) ]
        , locationType = Set.fromList [ LocIron , LocContact ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 1 , A Iron ) ]
              (Gain 3 ContactGrey)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Labor Camp"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocWorker ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 2 Worker) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Landfill"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) ]
        , locationType = Set.fromList [ LocAmmo ]
        , locationText = Do (Gain 1 Ammo)
        , locationBuildBonus = Do (Gain 1 Ammo)
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Lemmy's Storage"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocAmmo ]
        , locationText = Do (Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Market Place"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) ]
        , locationType = Set.fromList [ LocAmmo , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 2 , AnyBasicResource ) ]
              (Gain 2 VP)
              (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Material Depository"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Iron ) , ( 1 , Fuel ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 2 , A Fuel ) , ( 2 , A Iron ) ]
              (Gain 4 VP)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Merc Outpost"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocAmmo , LocWorker ]
        , locationText =
            Act [ ( 1 , AnyBasicResource ) ] (Gain 2 Worker) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Mesmerizers' Dwelling"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText = EachTime RazedSomething 1 VP
        , locationBuildBonus = None
        , locationDeal = VP
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Methane Storage "
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) ]
        , locationType = Set.fromList [ LocFuel ]
        , locationText = None
        , locationBuildBonus = OneTimeCache [ ( 3 , Fuel ) ]
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Military Warehouse"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Gun ) , ( 1 , Fuel ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 2 , A Fuel ) , ( 2 , A Gun ) ]
              (Gain 4 VP)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Mobile Storage"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker )
              , ( 1 , A Brick )
              , ( 1 , A Fuel )
              , ( 1 , A Gun )
              ]
              (Gain 3 VP)
              (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Motel"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , NewCard ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocCard ]
        , locationText =
            Act [ ( 1 , A Worker ) ] (Gain 1 NewCard) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = NewCard
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Murderers Pub"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) , ( 1 , Gun ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocGun 1 VP (Limit 5)
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Musem"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocLiberty 1 VP (Limit 5)
        , locationDeal = VP
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Natural Shelters"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocWorker , LocAmmo ]
        , locationText = Do (Gain 1 Worker `AndAlso` Gain 1 Ammo)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Negotiator"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 1 , A Deal ) ] (Gain 2 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Ohio Cavalry"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped
            [ ( 1 , Fuel ) , ( 1 , ContactBlue ) , ( 1 , Worker ) ]
        , locationType = Set.fromList [ LocFuel , LocContact ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 1 , A Fuel ) ]
              (Gain 3 ContactBlue)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = ContactBlue
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Oil Rig"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Fuel ) ]
        , locationType = Set.fromList [ LocFuel , LocAmmo ]
        , locationText = ForEachLoc LocFuel 1 Fuel (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Oil Trader"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText = Act [ ( 1 , A Fuel ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Oilfield"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) ]
        , locationType = Set.fromList [ LocFuel ]
        , locationText = Act [ ( 1 , A Worker ) ] (Gain 2 Fuel) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Oilmen"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , ContactBlue ) ]
        , locationType = Set.fromList [ LocFuel , LocContact ]
        , locationText = EachTime (Produced 1 (A Fuel)) 1 ContactBlue
        , locationBuildBonus = None
        , locationDeal = ContactBlue
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Oilmen  Fortress"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Fuel ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 2 , A Fuel ) ] (Gain 3 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Old Cinema"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Do (Gain 1 VP)
        , locationDeal = VP
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Old Cinema"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Do (Gain 1 VP)
        , locationDeal = VP
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Old Settlements"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , Iron ) ]
        , locationType = Set.fromList [ LocIron , LocWorker ]
        , locationText = Act [ ( 1 , A Iron ) ] (Gain 1 Worker) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Outpost Mercenaries"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Fuel ) , ( 1 , Gun ) ]
        , locationType = Set.fromList [ LocContact ]
        , locationText =
            Do (Choose (Gain 1 ContactRed) (Gain 1 ContactBlue))
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Parking Lot"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , Iron ) ]
        , locationType = Set.fromList [ LocIron , LocAmmo ]
        , locationText = ForEachLoc LocIron 1 Iron (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Patriot Tower"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocVictory , LocWorker ]
        , locationText = EachTime (Produced 5 (A Worker)) 2 VP
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Pete's Office"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocVictory , LocFuel ]
        , locationText = EachTime MadeADeal 1 VP
        , locationBuildBonus = None
        , locationDeal = VP
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Pickers"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocWorker , LocContact ]
        , locationText =
            Act [ ( 1 , A ContactBlue ) ] (Gain 1 Worker) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Post Office"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactBlue ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocContact , LocFuel ]
        , locationText =
            Act [ ( 1 , A Worker ) ] (Gain 1 ContactBlue) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = ContactBlue
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Power Station"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) , ( 1 , Worker ) ]
        , locationType = Set.fromList [ LocAmmo ]
        , locationText = Do (ActivateProduction Yours 1)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Preacher of the New Era"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped
            [ ( 1 , Gun ) , ( 1 , ContactRed ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocFuel , LocContact ]
        , locationText =
            Act [ ( 1 , A ContactBlue ) ] (Gain 1 ContactRed) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Manager"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , NewCard ) , ( 1 , Ammo ) ]
        , locationType = Set.fromList [ LocToxic , LocAmmo ]
        , locationText =
            Act [ ( 2 , A Worker ) ] (ActivateProduction Yours 1) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Propaganda Station"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , NewCard ) ]
        , locationType = Set.fromList [ LocCard ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 1 , A NewCard ) ] (Gain 2 NewCard) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = NewCard
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Pub"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , NewCard ) ]
        , locationType = Set.fromList [ LocCard ]
        , locationText = Do (Gain 1 NewCard)
        , locationBuildBonus = Do (Gain 1 NewCard)
        , locationDeal = NewCard
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Pub"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 3 , NewCard ) ]
        , locationType = Set.fromList [ LocCard ]
        , locationText = Do (Gain 1 NewCard)
        , locationBuildBonus = Do (Gain 1 NewCard)
        , locationDeal = NewCard
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Quarry"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) ]
        , locationType = Set.fromList [ LocBrick , LocAmmo ]
        , locationText = Do (Gain 1 Brick)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Radioactive Colony"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) , ( 1 , Worker ) ]
        , locationType = Set.fromList []
        , locationText = None
        , locationBuildBonus =
            OneTimeCache
              [ ( 1 , Worker )
              , ( 2 , Ammo )
              , ( 1 , ContactBlue )
              , ( 1 , ContactRed )
              ]
        , locationDeal = Ammo
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Radioactive Fuel"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Fuel ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 1 , A Fuel ) ] (Gain 2 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Refinery "
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) , ( 1 , Fuel ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocFuel 1 VP (Limit 5)
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Rehabilitation Center"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocWorker , LocContact ]
        , locationText =
            Act [ ( 1 , A ContactRed ) ] (Gain 1 Worker) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Ricky The Merchant "
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Fuel ) , ( 1 , Worker ) ]
        , locationType = Set.fromList [ LocFuel , LocContact ]
        , locationText = Do (Gain 1 ContactBlue)
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = NewEra
        }
    )
  , ( 2
    , Location
        { locationName = "Rifle"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , NewCard ) ]
        , locationType = Set.fromList [ LocCard , LocGun ]
        , locationText = EachTime RazedSomething 1 NewCard
        , locationBuildBonus = None
        , locationDeal = NewCard
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Rubble  "
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) , ( 1 , Brick ) ]
        , locationType = Set.fromList [ LocBrick , LocVictory ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocBrick 1 VP (Limit 5)
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Rubble Trader"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Brick ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocBrick , LocVictory ]
        , locationText = Act [ ( 1 , A Brick ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Ruined Library"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Gun ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocGun , LocAmmo ]
        , locationText = Storage [ ( Limit 3 , AnyBasicResource ) ]
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Salt Lake Tower"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Brick ) , ( 1 , Iron ) ]
        , locationType = Set.fromList [ LocIron , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 2 , A Iron ) , ( 2 , A Brick ) ]
              (Gain 4 VP)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = Winter
        }
    )
  , ( 3
    , Location
        { locationName = "School"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) ]
        , locationType = Set.fromList [ LocWorker , LocAmmo ]
        , locationText = Do (Gain 1 Worker)
        , locationBuildBonus = Do (Gain 1 Worker)
        , locationDeal = Worker
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Scrap Metal"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) ]
        , locationType = Set.fromList [ LocIron ]
        , locationText = None
        , locationBuildBonus = OneTimeCache [ ( 3 , Iron ) ]
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Scrap Trader"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocIron , LocVictory ]
        , locationText = Act [ ( 1 , A Iron ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Secret Outpost"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , Gun ) ]
        , locationType = Set.fromList [ LocWorker , LocGun ]
        , locationText = Act [ ( 1 , A Gun ) ] (Gain 1 Worker) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Shadow"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped
            [ ( 1 , ContactRed ) , ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText = Act [ ( 1 , A ContactRed ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Sharrash"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactUniversal ) ]
        , locationType = Set.fromList [ LocContact ]
        , locationText = Do (Gain 1 ContactUniversal)
        , locationBuildBonus = Do (Gain 1 ContactUniversal)
        , locationDeal = ContactUniversal
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Shelter"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Iron ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocIron ]
        , locationText = Act [ ( 2 , A Iron ) ] (Gain 2 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Sheriff's Office"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText = ForEachProduced Gun 1 VP (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Shipwreck"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocIron , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 1 , A Iron ) ] (Gain 2 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Sky Scraper"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Brick ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocBrick , LocVictory ]
        , locationText =
            Act [ ( 1 , A Worker ) , ( 2 , A Brick ) ] (Gain 3 VP) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Slave Hunters"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocWorker , LocGun ]
        , locationText = EachTime RazedSomething 1 Worker
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Slave Merchant"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Worker ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocWorker , LocFuel ]
        , locationText = EachTime MadeADeal 1 Worker
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Small Factory"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Iron ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocIron , LocVictory ]
        , locationText = ForEachProduced Iron 1 VP (Limit 3)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Solitary Trader"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocFuel , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker )
              , ( 1 , A Brick )
              , ( 1 , A Fuel )
              , ( 1 , A Gun )
              , ( 1 , A Iron )
              ]
              (Gain 3 VP)
              (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Steel Eagle"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) ]
        , locationType = Set.fromList [ LocLiberty , LocVictory ]
        , locationText = Do (Gain 1 VP)
        , locationBuildBonus = None
        , locationDeal = VP
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Storehouse"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Gun ) , ( 1 , Fuel ) ]
        , locationType = Set.fromList [ LocFuel , LocGun ]
        , locationText = Do (Choose (Gain 1 Fuel) (Gain 1 Gun))
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Suppliers"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactGrey ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocAmmo , LocToxic ]
        , locationText = Do (ActivateProduction Others 1)
        , locationBuildBonus = Do (ActivateProduction Others 1)
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Tanker Shipwreck"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Fuel ) , ( 1 , Brick ) ]
        , locationType = Set.fromList [ LocFuel , LocBrick ]
        , locationText = Do (Choose (Gain 1 Brick) (Gain 1 Fuel))
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Ted's Bus"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactGrey ) ]
        , locationType = Set.fromList [ LocIron , LocContact ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 1 , AnyBasicResource ) ]
              (Gain 2 ContactGrey)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "The Bronx Gang"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped
            [ ( 1 , Gun ) , ( 1 , ContactRed ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocContact , LocGun ]
        , locationText =
            Act
              [ ( 3 , A Worker ) , ( 1 , A Gun ) ] (Gain 3 ContactRed) (Limit 1)
        , locationBuildBonus = None
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "The Hold"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Gun ) , ( 1 , Brick ) ]
        , locationType = Set.fromList [ LocBrick , LocVictory ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 2 , A Gun ) , ( 2 , A Brick ) ]
              (Gain 4 VP)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Brick
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "The Iron Gang"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Gun ) , ( 1 , Worker ) ]
        , locationType = Set.fromList [ LocGun , LocContact ]
        , locationText = Do (Gain 3 ContactRed)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Thieves Caravan"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 3 , Ammo ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocAmmo , LocGun ]
        , locationText = Act [ ( 1 , A Worker ) ] (StealBasic 1) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Thieves Den"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Ammo ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocWorker , LocAmmo ]
        , locationText =
            Storage [ ( Limit 2 , AnyBasicResource ) , ( Limit 2 , A Worker ) ]
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Training camp"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped
            [ ( 1 , ContactRed ) , ( 1 , Worker ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocContact , LocGun ]
        , locationText =
            Act [ ( 1 , A Worker ) ] (Gain 1 ContactRed) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Transmitter"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , NewCard ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocCard ]
        , locationText =
            Act
              [ ( 1 , A Worker ) , ( 1 , AnyBasicResource ) ]
              (Gain 2 NewCard)
              (Limit 1)
        , locationBuildBonus = None
        , locationDeal = NewCard
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Truck"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) ]
        , locationType = Set.fromList [ LocContact , LocAmmo ]
        , locationText = EachTime (Either RazedSomething MadeADeal) 1 Ammo
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Underground Warehouse"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Fuel ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocFuel , LocAmmo ]
        , locationText = Storage [ ( Limit 3 , AnyBasicResource ) ]
        , locationBuildBonus = None
        , locationDeal = Fuel
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Weapon Trader"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , Gun ) , ( 1 , NewCard ) ]
        , locationType = Set.fromList [ LocGun , LocVictory ]
        , locationText = Act [ ( 1 , A Gun ) ] (Gain 1 VP) (Limit 2)
        , locationBuildBonus = None
        , locationDeal = Gun
        , locationSet = BaseSet
        }
    )
  , ( 2
    , Location
        { locationName = "Wire Entanglement"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 3 , Shield ) ]
        , locationType = Set.fromList [ LocGun ]
        , locationText = Do (Gain 1 Shield)
        , locationBuildBonus = Do (Gain 1 Shield)
        , locationDeal = Shield
        , locationSet = Winter
        }
    )
  , ( 3
    , Location
        { locationName = "Wrecked Tank"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocLiberty 1 VP (Limit 5)
        , locationDeal = VP
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Wrecked Tank"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocLiberty 1 VP (Limit 5)
        , locationDeal = VP
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Wrecker"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactGrey ) ]
        , locationType = Set.fromList [ LocIron , LocContact ]
        , locationText = EachTime (Produced 1 (A Iron)) 1 ContactGrey
        , locationBuildBonus = None
        , locationDeal = Iron
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Fixer"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , Ammo ) , ( 1 , Worker ) ]
        , locationType = Set.fromList [ LocToxic ]
        , locationText =
            Act [ ( 1 , A Worker ) ] UseBlockedOpenProduction (Limit 1)
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = PromoSet1
        }
    )
  , ( 1
    , Location
        { locationName = "Secret Stock"
        , locationClass = Feature
        , locationDistance = 3
        , locationSpoils = bagFromListGrouped [ ( 2 , Ammo ) , ( 2 , VP ) ]
        , locationType = Set.fromList [ LocToxic , LocAmmo ]
        , locationText = SecretStock
        , locationBuildBonus = None
        , locationDeal = Ammo
        , locationSet = PromoSet1
        }
    )
  , ( 1
    , Location
        { locationName = "Propaganda Center"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = bagFromListGrouped [ ( 1 , NewCard ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocCard ]
        , locationText = Act [ ( 2 , A NewCard ) ] Draw2Keep1 (Limit 1)
        , locationBuildBonus = None
        , locationDeal = NewCard
        , locationSet = PromoSet1
        }
    )
  , ( 1
    , Location
        { locationName = "Wrecked Airplane"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , VP ) ]
        , locationType = Set.fromList [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = ForEachLoc LocLiberty 1 VP (Limit 5)
        , locationDeal = VP
        , locationSet = PromoSet1
        }
    )
  , ( 1
    , Location
        { locationName = "Armored Car"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = bagFromListGrouped [ ( 2 , ContactRed ) , ( 1 , VP ) ]
        , locationType = Set.fromList [ LocGun ]
        , locationText = DecreaseDefence 1
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = PromoSet1
        }
    )
  ]
