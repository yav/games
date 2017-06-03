module Cards where

import Types

connections :: [(Int,Connection)]
connections =
  [ ( 2
    , Connection
        { connectionName = "Junk Train"
        , connectionText = Convert [] [ ( 3 , ContactBlue ) ] 1
        , connectionSet = BaseSet
        }
    )
  , ( 4
    , Connection
        { connectionName = "Merchants"
        , connectionText = Convert [] [ ( 2 , ContactBlue ) ] 1
        , connectionSet = BaseSet
        }
    )
  , ( 4
    , Connection
        { connectionName = "Punks"
        , connectionText = Convert [] [ ( 2 , ContactRed ) ] 1
        , connectionSet = BaseSet
        }
    )
  , ( 2
    , Connection
        { connectionName = "Thugs"
        , connectionText =
            Convert [ ( 1 , AnyOf [ Gun ] ) ] [ ( 3 , ContactRed ) ] 1
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
        , locationSpoils = [ ( 3 , Brick ) , ( 1 , VP ) ]
        , locationType = [ LocBrick ]
        , locationText =
            Convert [ ( 2 , AnyOf [ Brick ] ) ] [ ( 2 , VP ) ] 2
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocWorker , LocVictory ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 1 , VP ) ] 2
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
        , locationSpoils = [ ( 3 , Gun ) ]
        , locationType = [ LocGun , LocAmmo ]
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
        , locationSpoils = [ ( 2 , Ammo ) , ( 1 , VP ) ]
        , locationType = [ LocVictory , LocAmmo ]
        , locationText =
            IfProducedAtLeast 5 (AnyOf [ Brick , Fuel , Gun , Iron ]) 2 VP
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
        , locationSpoils = [ ( 2 , Gun ) ]
        , locationType = [ LocGun , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Gun ) ] 1
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
        , locationSpoils = [ ( 3 , Iron ) , ( 1 , VP ) ]
        , locationType = [ LocIron , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 2 , AnyOf [ Iron ] ) ]
              [ ( 3 , VP ) ]
              1
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
        , locationSpoils = [ ( 2 , Gun ) , ( 1 , VP ) ]
        , locationType = [ LocGun , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ Gun ] ) ]
              [ ( 2 , VP ) ]
              1
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
        , locationSpoils = [ ( 1 , Iron ) , ( 1 , Gun ) ]
        , locationType = [ LocIron , LocGun ]
        , locationText = Convert [] [ ( 1 , Iron ) , ( 1 , Gun ) ] 1
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
        , locationSpoils = [ ( 3 , Ammo ) ]
        , locationType = [ LocBrick , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 1 , AnyOf [ Brick ] )
              , ( 1 , AnyOf [ Iron ] )
              , ( 1 , AnyOf [ Gun ] )
              ]
              [ ( 3 , VP ) ]
              2
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
        , locationSpoils = [ ( 2 , ContactBlue ) ]
        , locationType = [ LocContact , LocFuel ]
        , locationText = Storage [ ( Limit 3 , AnyOf [ ContactBlue ] ) ]
        , locationBuildBonus = Convert [] [ ( 1 , ContactBlue ) ] 1
        , locationDeal = ContactBlue
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Body Hunters"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = [ ( 3 , Worker ) ]
        , locationType = [ LocWorker , LocToxic ]
        , locationText =
            Other
              [ LexW2 "spend"
              , LexR2 1 (Resource Worker)
              , LexW2 "to"
              , LexW2 "choose"
              , LexW2 "up"
              , LexW2 "to"
              , LexR2 3 YourLocation
              , LexW2 "and"
              , LexW2 "remove"
              , LexW2 "all"
              , LexW2 "of"
              , LexW2 "the"
              , LexW2 "goods"
              , LexW2 "and"
              , LexW2 "workers"
              , LexW2 "from"
              , LexW2 "them"
              ]
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
        , locationSpoils = [ ( 3 , Worker ) ]
        , locationType = [ LocWorker ]
        , locationText = Convert [] [ ( 2 , Worker ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , Worker ) ] 1
        , locationDeal = Worker
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Boiler Room"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = [ ( 3 , Worker ) ]
        , locationType = [ LocWorker ]
        , locationText = Convert [] [ ( 2 , Worker ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , Worker ) ] 1
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Brick Storage"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = [ ( 2 , Brick ) ]
        , locationType = [ LocBrick ]
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
        , locationSpoils = [ ( 3 , Brick ) ]
        , locationType = [ LocBrick , LocAmmo ]
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
        , locationSpoils = [ ( 2 , Brick ) ]
        , locationType = [ LocBrick ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 2 , Brick ) ] 1
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , Brick ) ]
        , locationType = [ LocBrick , LocWorker ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Brick ] ) ] [ ( 1 , Worker ) ] 2
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , NewCard ) ]
        , locationType = [ LocWorker , LocFuel ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "make"
              , LexW2 "a"
              , LexW2 "deal"
              , LexW2 "gain"
              , LexR2 1 (Resource Worker)
              ]
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
        , locationSpoils = [ ( 2 , Ammo ) , ( 1 , Worker ) ]
        , locationType = [ LocGun , LocContact ]
        , locationText =
            Other
              [ LexW2 "spend"
              , LexR2 1 (Resource Worker)
              , LexW2 "to"
              , LexW2 "choose"
              , LexR2 1 AnothersDeals
              , LexW2 "and"
              , LexW2 "gain"
              , LexW2 "the"
              , LexW2 "goods"
              , LexW2 "it"
              , LexW2 "provides"
              ]
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocWorker ]
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
        , locationSpoils =
            [ ( 1 , ContactRed ) , ( 1 , Gun ) , ( 1 , Worker ) ]
        , locationType = [ LocToxic , LocGun ]
        , locationText = Convert [] [ ( 1 , Shield ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , Shield ) ] 1
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Caravanner"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = [ ( 3 , Ammo ) ]
        , locationType = [ LocFuel , LocToxic ]
        , locationText =
            Other
              [ LexW2 "spend"
              , LexR2 1 (Resource Worker)
              , LexW2 "to"
              , LexW2 "choose"
              , LexR2 3 YourDeals
              , LexW2 "and"
              , LexW2 "gain"
              , LexW2 "the"
              , LexW2 "goods"
              , LexW2 "they"
              , LexW2 "produce"
              ]
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
        , locationSpoils = [ ( 2 , VP ) ]
        , locationType = [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Convert [] [ ( 2 , VP ) ] 1
        , locationDeal = VP
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Church"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = [ ( 2 , VP ) ]
        , locationType = [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Convert [] [ ( 2 , VP ) ] 1
        , locationDeal = VP
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Cistern"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = [ ( 2 , Fuel ) , ( 1 , VP ) ]
        , locationType = [ LocFuel , LocVictory ]
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
        , locationSpoils = [ ( 2 , Gun ) ]
        , locationType = [ LocGun ]
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = [ LocBrick , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ Brick ] ) ]
              [ ( 2 , VP ) ]
              1
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
        , locationSpoils = [ ( 3 , Shield ) ]
        , locationType = [ LocGun , LocContact ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 1 , Shield ) ] 1
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
        , locationSpoils = [ ( 2 , Gun ) ]
        , locationType = [ LocGun ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 2 , Gun ) ] 1
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
        , locationSpoils = [ ( 3 , Gun ) , ( 1 , VP ) ]
        , locationType = [ LocGun ]
        , locationText = Convert [ ( 2 , AnyOf [ Gun ] ) ] [ ( 2 , VP ) ] 2
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = [ LocBrick , LocVictory ]
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
        , locationSpoils = [ ( 2 , ContactGrey ) ]
        , locationType = [ LocIron , LocContact ]
        , locationText = Convert [] [ ( 1 , ContactGrey ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , ContactGrey ) ] 1
        , locationDeal = Iron
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Convoy"
        , locationClass = Action
        , locationDistance = 1
        , locationSpoils = [ ( 2 , ContactBlue ) ]
        , locationType = [ LocFuel , LocContact ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Fuel ] ) ] [ ( 1 , ContactBlue ) ] 2
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
        , locationSpoils = [ ( 2 , Ammo ) ]
        , locationType = [ LocAmmo , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 1 , AnyOf [ Brick , Fuel , Gun , Iron ] )
              ]
              [ ( 1 , VP ) ]
              2
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , Ammo ) ]
        , locationType = [ LocBrick , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Brick ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , Ammo ) ]
        , locationType = [ LocBrick , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Brick ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 1 , Iron ) , ( 1 , Brick ) ]
        , locationType = [ LocIron , LocBrick ]
        , locationText =
            Other
              [ LexR2 1 (Resource Brick) , LexW2 "or" , LexR2 1 (Resource Iron) ]
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
        , locationSpoils = [ ( 3 , ContactGrey ) ]
        , locationType = [ LocContact ]
        , locationText = Storage [ ( Limit 3 , AnyOf [ ContactGrey ] ) ]
        , locationBuildBonus = Convert [] [ ( 4 , ContactGrey ) ] 1
        , locationDeal = ContactGrey
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Crossroads"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = [ ( 2 , NewCard ) ]
        , locationType = [ LocWorker , LocCard ]
        , locationText = None
        , locationBuildBonus = Convert [] [ ( 2 , NewCard ) ] 1
        , locationDeal = NewCard
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Dark Visions Gang"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = [ ( 2 , ContactRed ) , ( 1 , VP ) ]
        , locationType = [ LocGun , LocContact ]
        , locationText = IfProducedAtLeast 1 (AnyOf [ Gun ]) 1 ContactRed
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
        , locationSpoils = [ ( 1 , Development ) , ( 1 , Brick ) ]
        , locationType = [ LocBrick , LocContact ]
        , locationText =
            IfProducedAtLeast 1 (AnyOf [ Brick ]) 1 Development
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
        , locationSpoils = [ ( 2 , Ammo ) , ( 1 , VP ) ]
        , locationType = [ LocAmmo ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 1 , Ammo ) ] 2
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
        , locationSpoils = [ ( 2 , Iron ) ]
        , locationType = [ LocIron ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 2 , Iron ) ] 1
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
        , locationSpoils = [ ( 2 , Iron ) ]
        , locationType = [ LocIron , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Iron ) ] 1
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
        , locationSpoils = [ ( 3 , Worker ) ]
        , locationType = [ LocVictory , LocWorker ]
        , locationText = IfProducedAtLeast 2 (AnyOf [ VP ]) 1 Worker
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
        , locationSpoils = [ ( 3 , Ammo ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 1 , AnyOf [ Fuel ] )
              , ( 1 , AnyOf [ Brick ] )
              , ( 1 , AnyOf [ Iron ] )
              ]
              [ ( 3 , VP ) ]
              2
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
        , locationSpoils = [ ( 2 , NewCard ) , ( 1 , VP ) ]
        , locationType = [ LocFuel , LocCard ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "make"
              , LexW2 "a"
              , LexW2 "deal"
              , LexW2 "gain"
              , LexR2 1 (Resource NewCard)
              ]
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = [ LocBrick , LocContact ]
        , locationText = Convert [] [ ( 1 , Development ) ] 1
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = [ LocBrick , LocContact ]
        , locationText = Convert [] [ ( 1 , Development ) ] 1
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , VP ) ]
        , locationType = [ LocBrick , LocContact ]
        , locationText = Convert [] [ ( 1 , Development ) ] 1
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
        , locationSpoils = [ ( 2 , Gun ) , ( 1 , Ammo ) ]
        , locationType = [ LocGun , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Gun ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , Gun ) , ( 1 , Ammo ) ]
        , locationType = [ LocGun , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Gun ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , VP ) , ( 1 , Iron ) ]
        , locationType = [ LocIron , LocVictory ]
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
        , locationSpoils = [ ( 2 , ContactGrey ) ]
        , locationType = []
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
        , locationSpoils = [ ( 2 , Fuel ) ]
        , locationType = [ LocFuel , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Fuel ) ] 1
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
        , locationSpoils = [ ( 2 , ContactRed ) ]
        , locationType = [ LocContact , LocFuel ]
        , locationText = Storage [ ( Limit 3 , AnyOf [ ContactRed ] ) ]
        , locationBuildBonus = Convert [] [ ( 1 , ContactRed ) ] 1
        , locationDeal = ContactRed
        , locationSet = NewEra
        }
    )
  , ( 1
    , Location
        { locationName = "Gasoline Cultist"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils =
            [ ( 1 , ContactBlue ) , ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Convert [ ( 1 , AnyOf [ ContactBlue ] ) ] [ ( 1 , VP ) ] 2
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
        , locationSpoils = [ ( 3 , Fuel ) , ( 1 , VP ) ]
        , locationType = [ LocFuel ]
        , locationText =
            Convert [ ( 2 , AnyOf [ Fuel ] ) ] [ ( 2 , VP ) ] 2
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , Fuel ) ]
        , locationType = [ LocWorker , LocFuel ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Fuel ] ) ] [ ( 1 , Worker ) ] 2
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
        , locationSpoils = [ ( 2 , NewCard ) ]
        , locationType = [ LocFuel , LocCard ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "make"
              , LexW2 "a"
              , LexW2 "deal"
              , LexW2 "gain"
              , LexR2 1 (Resource NewCard)
              ]
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
        , locationSpoils = [ ( 3 , Gun ) , ( 1 , VP ) ]
        , locationType = [ LocGun , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 2 , AnyOf [ Gun ] ) ]
              [ ( 3 , VP ) ]
              1
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
        , locationSpoils = [ ( 2 , ContactRed ) ]
        , locationType = [ LocGun , LocContact ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Gun ] ) ] [ ( 1 , ContactRed ) ] 2
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
        , locationSpoils = [ ( 2 , Fuel ) , ( 1 , Ammo ) ]
        , locationType = [ LocFuel , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Fuel ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , Fuel ) , ( 1 , Ammo ) ]
        , locationType = [ LocFuel , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Fuel ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , Iron ) , ( 1 , Ammo ) ]
        , locationType = [ LocIron , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Iron ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , Iron ) , ( 1 , Ammo ) ]
        , locationType = [ LocIron , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Iron ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocWorker ]
        , locationText = Storage [ ( NoLimit , AnyOf [ Worker ] ) ]
        , locationBuildBonus = Convert [] [ ( 4 , Worker ) ] 1
        , locationDeal = Worker
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Hidden Forge"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocToxic , LocGun ]
        , locationText = Convert [] [ ( 1 , Shield ) ] 1
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
        , locationSpoils = [ ( 2 , VP ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Other
              [ LexW2 "discard"
              , LexR2 1 YourDeals
              , LexW2 "to"
              , LexW2 "gain"
              , LexR2 1 (Resource VP)
              , ActivatedTwice
              ]
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
        , locationSpoils = [ ( 2 , ContactGrey ) ]
        , locationType = [ LocIron , LocContact ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ Iron ] ) ]
              [ ( 3 , ContactGrey ) ]
              1
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocWorker ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 2 , Worker ) ] 1
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
        , locationSpoils = [ ( 3 , Ammo ) ]
        , locationType = [ LocAmmo ]
        , locationText = Convert [] [ ( 1 , Ammo ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , Ammo ) ] 1
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Lemmy's Storage"
        , locationClass = Production Closed
        , locationDistance = 2
        , locationSpoils = [ ( 2 , Ammo ) , ( 1 , VP ) ]
        , locationType = [ LocAmmo ]
        , locationText = Convert [] [ ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 2 , Ammo ) ]
        , locationType = [ LocAmmo , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 2 , AnyOf [ Brick , Fuel , Gun , Iron ] )
              ]
              [ ( 2 , VP ) ]
              2
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
        , locationSpoils = [ ( 1 , Iron ) , ( 1 , Fuel ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 2 , AnyOf [ Fuel ] )
              , ( 2 , AnyOf [ Iron ] )
              ]
              [ ( 4 , VP ) ]
              1
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocAmmo , LocWorker ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Brick , Fuel , Gun , Iron ] ) ]
              [ ( 2 , Worker ) ]
              1
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
        , locationSpoils = [ ( 2 , VP ) , ( 1 , NewCard ) ]
        , locationType = [ LocGun , LocVictory ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "raze"
              , LexW2 "gain"
              , LexR2 1 (Resource VP)
              ]
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
        , locationSpoils = [ ( 2 , Fuel ) ]
        , locationType = [ LocFuel ]
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
        , locationSpoils = [ ( 1 , Gun ) , ( 1 , Fuel ) ]
        , locationType = [ LocGun , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 2 , AnyOf [ Fuel ] )
              , ( 2 , AnyOf [ Gun ] )
              ]
              [ ( 4 , VP ) ]
              1
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
        , locationSpoils = [ ( 3 , Ammo ) ]
        , locationType = [ LocGun , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 1 , AnyOf [ Brick ] )
              , ( 1 , AnyOf [ Fuel ] )
              , ( 1 , AnyOf [ Gun ] )
              ]
              [ ( 3 , VP ) ]
              2
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
        , locationSpoils = [ ( 2 , NewCard ) , ( 1 , VP ) ]
        , locationType = [ LocCard ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 1 , NewCard ) ] 2
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
        , locationSpoils = [ ( 2 , VP ) , ( 1 , Gun ) ]
        , locationType = [ LocGun , LocVictory ]
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
        , locationSpoils = [ ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = [ LocLiberty ]
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , Ammo ) ]
        , locationType = [ LocWorker , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Worker ) , ( 1 , Ammo ) ] 1
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
        , locationSpoils = [ ( 3 , Ammo ) , ( 1 , NewCard ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Other
              [ LexW2 "spend"
              , LexR2 1 (Resource Worker)
              , LexW2 "and"
              , LexW2 "discard"
              , LexR2 1 YourDeals
              , LexW2 "to"
              , LexW2 "gain"
              , LexR2 2 (Resource VP)
              ]
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
        , locationSpoils =
            [ ( 1 , Fuel ) , ( 1 , ContactBlue ) , ( 1 , Worker ) ]
        , locationType = [ LocFuel , LocContact ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ Fuel ] ) ]
              [ ( 3 , ContactBlue ) ]
              1
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
        , locationSpoils = [ ( 3 , Fuel ) ]
        , locationType = [ LocFuel , LocAmmo ]
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
        , locationSpoils = [ ( 2 , Fuel ) , ( 1 , NewCard ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Fuel ] ) ] [ ( 1 , VP ) ] 2
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
        , locationSpoils = [ ( 2 , Fuel ) ]
        , locationType = [ LocFuel ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 2 , Fuel ) ] 1
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
        , locationSpoils = [ ( 3 , ContactBlue ) ]
        , locationType = [ LocFuel , LocContact ]
        , locationText = IfProducedAtLeast 1 (AnyOf [ Fuel ]) 1 ContactBlue
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
        , locationSpoils = [ ( 3 , Fuel ) , ( 1 , VP ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 2 , AnyOf [ Fuel ] ) ]
              [ ( 3 , VP ) ]
              1
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
        , locationSpoils = [ ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Convert [] [ ( 1 , VP ) ] 1
        , locationDeal = VP
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Old Cinema"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = [ ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = [ LocLiberty ]
        , locationText = None
        , locationBuildBonus = Convert [] [ ( 1 , VP ) ] 1
        , locationDeal = VP
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Old Settlements"
        , locationClass = Action
        , locationDistance = 2
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , Iron ) ]
        , locationType = [ LocIron , LocWorker ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Iron ] ) ] [ ( 1 , Worker ) ] 2
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
        , locationSpoils = [ ( 1 , Fuel ) , ( 1 , Gun ) ]
        , locationType = [ LocContact ]
        , locationText =
            Other
              [ LexR2 1 (Resource ContactRed)
              , LexW2 "or"
              , LexR2 1 (Resource ContactBlue)
              ]
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
        , locationSpoils = [ ( 3 , Iron ) ]
        , locationType = [ LocIron , LocAmmo ]
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , VP ) ]
        , locationType = [ LocVictory , LocWorker ]
        , locationText = IfProducedAtLeast 5 (AnyOf [ Worker ]) 2 VP
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
        , locationSpoils = [ ( 2 , VP ) , ( 1 , NewCard ) ]
        , locationType = [ LocVictory , LocFuel ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "make"
              , LexW2 "a"
              , LexW2 "deal"
              , LexW2 "gain"
              , LexR2 1 (Resource VP)
              ]
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocWorker , LocContact ]
        , locationText =
            Convert [ ( 1 , AnyOf [ ContactBlue ] ) ] [ ( 1 , Worker ) ] 1
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
        , locationSpoils = [ ( 2 , ContactBlue ) , ( 1 , NewCard ) ]
        , locationType = [ LocContact , LocFuel ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 1 , ContactBlue ) ] 2
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
        , locationSpoils = [ ( 2 , Ammo ) , ( 1 , Worker ) ]
        , locationType = [ LocAmmo ]
        , locationText =
            Other
              [ LexW2 "choose"
              , LexW2 "one"
              , LexW2 "of"
              , LexW2 "your"
              , LexW2 "other"
              , LexW2 "production"
              , LexW2 "location"
              , LexW2 "and"
              , LexW2 "gain"
              , LexW2 "the"
              , LexW2 "goods"
              , LexW2 "it"
              , LexW2 "produces"
              ]
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
        , locationSpoils =
            [ ( 1 , Gun ) , ( 1 , ContactRed ) , ( 1 , NewCard ) ]
        , locationType = [ LocFuel , LocContact ]
        , locationText =
            Convert [ ( 1 , AnyOf [ ContactBlue ] ) ] [ ( 1 , ContactRed ) ] 2
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
        , locationSpoils = [ ( 2 , NewCard ) , ( 1 , Ammo ) ]
        , locationType = [ LocToxic , LocAmmo ]
        , locationText =
            Other
              [ LexW2 "spend"
              , LexR2 2 (Resource Worker)
              , LexW2 "to"
              , LexW2 "activate"
              , LexW2 "one"
              , LexW2 "of"
              , LexW2 "your"
              , LexW2 "production"
              , LexW2 "location"
              ]
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
        , locationSpoils = [ ( 3 , NewCard ) ]
        , locationType = [ LocCard ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ NewCard ] ) ]
              [ ( 2 , NewCard ) ]
              1
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
        , locationSpoils = [ ( 3 , NewCard ) ]
        , locationType = [ LocCard ]
        , locationText = Convert [] [ ( 1 , NewCard ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , NewCard ) ] 1
        , locationDeal = NewCard
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Pub"
        , locationClass = Production Open
        , locationDistance = 2
        , locationSpoils = [ ( 3 , NewCard ) ]
        , locationType = [ LocCard ]
        , locationText = Convert [] [ ( 1 , NewCard ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , NewCard ) ] 1
        , locationDeal = NewCard
        , locationSet = Winter
        }
    )
  , ( 2
    , Location
        { locationName = "Quarry"
        , locationClass = Production Open
        , locationDistance = 1
        , locationSpoils = [ ( 2 , Brick ) ]
        , locationType = [ LocBrick , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Brick ) ] 1
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
        , locationSpoils = [ ( 2 , Ammo ) , ( 1 , Worker ) ]
        , locationType = []
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
        , locationSpoils = [ ( 2 , Fuel ) , ( 1 , VP ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ Fuel ] ) ]
              [ ( 2 , VP ) ]
              1
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
        , locationSpoils = [ ( 2 , VP ) , ( 1 , Fuel ) ]
        , locationType = [ LocFuel , LocVictory ]
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocWorker , LocContact ]
        , locationText =
            Convert [ ( 1 , AnyOf [ ContactRed ] ) ] [ ( 1 , Worker ) ] 1
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
        , locationSpoils = [ ( 1 , Fuel ) , ( 1 , Worker ) ]
        , locationType = [ LocFuel , LocContact ]
        , locationText = Convert [] [ ( 1 , ContactBlue ) ] 1
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
        , locationSpoils = [ ( 2 , NewCard ) ]
        , locationType = [ LocCard , LocGun ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "raze"
              , LexW2 "gain"
              , LexR2 1 (Resource NewCard)
              ]
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
        , locationSpoils = [ ( 2 , VP ) , ( 1 , Brick ) ]
        , locationType = [ LocBrick , LocVictory ]
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
        , locationSpoils = [ ( 2 , Brick ) , ( 1 , NewCard ) ]
        , locationType = [ LocBrick , LocVictory ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Brick ] ) ] [ ( 1 , VP ) ] 2
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
        , locationSpoils = [ ( 1 , Gun ) , ( 1 , NewCard ) ]
        , locationType = [ LocGun , LocAmmo ]
        , locationText =
            Storage [ ( Limit 3 , AnyOf [ Brick , Fuel , Gun , Iron ] ) ]
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
        , locationSpoils = [ ( 1 , Brick ) , ( 1 , Iron ) ]
        , locationType = [ LocIron , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 2 , AnyOf [ Iron ] )
              , ( 2 , AnyOf [ Brick ] )
              ]
              [ ( 4 , VP ) ]
              1
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
        , locationSpoils = [ ( 2 , Worker ) ]
        , locationType = [ LocWorker , LocAmmo ]
        , locationText = Convert [] [ ( 1 , Worker ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , Worker ) ] 1
        , locationDeal = Worker
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Scrap Metal"
        , locationClass = Feature
        , locationDistance = 1
        , locationSpoils = [ ( 2 , Iron ) ]
        , locationType = [ LocIron ]
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
        , locationSpoils = [ ( 2 , Iron ) , ( 1 , NewCard ) ]
        , locationType = [ LocIron , LocVictory ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Iron ] ) ] [ ( 1 , VP ) ] 2
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , Gun ) ]
        , locationType = [ LocWorker , LocGun ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Gun ] ) ] [ ( 1 , Worker ) ] 2
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
        , locationSpoils =
            [ ( 1 , ContactRed ) , ( 1 , VP ) , ( 1 , NewCard ) ]
        , locationType = [ LocGun , LocVictory ]
        , locationText =
            Convert [ ( 1 , AnyOf [ ContactRed ] ) ] [ ( 1 , VP ) ] 2
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
        , locationSpoils = [ ( 2 , ContactUniversal ) ]
        , locationType = [ LocContact ]
        , locationText = Convert [] [ ( 1 , ContactUniversal ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , ContactUniversal ) ] 1
        , locationDeal = ContactUniversal
        , locationSet = BaseSet
        }
    )
  , ( 1
    , Location
        { locationName = "Shelter"
        , locationClass = Action
        , locationDistance = 3
        , locationSpoils = [ ( 3 , Iron ) , ( 1 , VP ) ]
        , locationType = [ LocIron ]
        , locationText =
            Convert [ ( 2 , AnyOf [ Iron ] ) ] [ ( 2 , VP ) ] 2
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
        , locationSpoils = [ ( 2 , Gun ) , ( 1 , VP ) ]
        , locationType = [ LocGun , LocVictory ]
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
        , locationSpoils = [ ( 2 , Iron ) , ( 1 , VP ) ]
        , locationType = [ LocIron , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ Iron ] ) ]
              [ ( 2 , VP ) ]
              1
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
        , locationSpoils = [ ( 3 , Brick ) , ( 1 , VP ) ]
        , locationType = [ LocBrick , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] ) , ( 2 , AnyOf [ Brick ] ) ]
              [ ( 3 , VP ) ]
              1
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , NewCard ) ]
        , locationType = [ LocWorker , LocGun ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "raze"
              , LexW2 "gain"
              , LexR2 1 (Resource Worker)
              ]
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
        , locationSpoils = [ ( 2 , Worker ) , ( 1 , NewCard ) ]
        , locationType = [ LocWorker , LocFuel ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "make"
              , LexW2 "a"
              , LexW2 "deal"
              , LexW2 "gain"
              , LexR2 1 (Resource Worker)
              ]
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
        , locationSpoils = [ ( 2 , Iron ) , ( 1 , VP ) ]
        , locationType = [ LocIron , LocVictory ]
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
        , locationSpoils = [ ( 3 , Ammo ) , ( 1 , VP ) ]
        , locationType = [ LocFuel , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 1 , AnyOf [ Brick ] )
              , ( 1 , AnyOf [ Fuel ] )
              , ( 1 , AnyOf [ Gun ] )
              , ( 1 , AnyOf [ Iron ] )
              ]
              [ ( 3 , VP ) ]
              2
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
        , locationSpoils = [ ( 2 , VP ) ]
        , locationType = [ LocLiberty , LocVictory ]
        , locationText = Convert [] [ ( 1 , VP ) ] 1
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
        , locationSpoils = [ ( 1 , Gun ) , ( 1 , Fuel ) ]
        , locationType = [ LocFuel , LocGun ]
        , locationText =
            Other
              [ LexR2 1 (Resource Fuel) , LexW2 "or" , LexR2 1 (Resource Gun) ]
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
        , locationSpoils = [ ( 2 , ContactGrey ) , ( 1 , VP ) ]
        , locationType = [ LocAmmo , LocToxic ]
        , locationText =
            Other
              [ LexW2 "chose"
              , LexW2 "another"
              , LexW2 "players"
              , LexW2 "production"
              , LexW2 "location"
              , LexW2 "and"
              , LexW2 "gain"
              , LexW2 "the"
              , LexW2 "goods"
              , LexW2 "it"
              , LexW2 "produces"
              , LexW2 "instead"
              , LexW2 "of"
              , LexW2 "them"
              ]
        , locationBuildBonus =
            Other
              [ LexW2 "choose"
              , LexW2 "another"
              , LexW2 "players"
              , LexW2 "production"
              , LexW2 "location"
              , LexW2 "and"
              , LexW2 "gain"
              , LexW2 "the"
              , LexW2 "goods"
              , LexW2 "it"
              , LexW2 "produces"
              ]
        , locationDeal = Ammo
        , locationSet = Winter
        }
    )
  , ( 1
    , Location
        { locationName = "Tanker Shipwreck"
        , locationClass = Production Closed
        , locationDistance = 1
        , locationSpoils = [ ( 1 , Fuel ) , ( 1 , Brick ) ]
        , locationType = [ LocFuel , LocBrick ]
        , locationText =
            Other
              [ LexR2 1 (Resource Brick) , LexW2 "or" , LexR2 1 (Resource Fuel) ]
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
        , locationSpoils = [ ( 2 , ContactGrey ) ]
        , locationType = [ LocIron , LocContact ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 1 , AnyOf [ Brick , Fuel , Gun , Iron ] )
              ]
              [ ( 2 , ContactGrey ) ]
              1
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
        , locationSpoils =
            [ ( 1 , Gun ) , ( 1 , ContactRed ) , ( 1 , NewCard ) ]
        , locationType = [ LocContact , LocGun ]
        , locationText =
            Convert
              [ ( 3 , AnyOf [ Worker ] ) , ( 1 , AnyOf [ Gun ] ) ]
              [ ( 3 , ContactRed ) ]
              1
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
        , locationSpoils = [ ( 1 , Gun ) , ( 1 , Brick ) ]
        , locationType = [ LocBrick , LocVictory ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 2 , AnyOf [ Gun ] )
              , ( 2 , AnyOf [ Brick ] )
              ]
              [ ( 4 , VP ) ]
              1
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
        , locationSpoils = [ ( 1 , Gun ) , ( 1 , Worker ) ]
        , locationType = [ LocGun , LocContact ]
        , locationText = Convert [] [ ( 3 , ContactRed ) ] 1
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
        , locationSpoils = [ ( 3 , Ammo ) , ( 1 , NewCard ) ]
        , locationType = [ LocAmmo , LocGun ]
        , locationText =
            Other
              [ LexW2 "spend"
              , LexR2 1 (Resource Worker)
              , LexW2 "to"
              , LexW2 "take"
              , LexR2 1 BasicResource
              , LexW2 "from"
              , LexW2 "another"
              , LexW2 "player"
              , ActivatedTwice
              ]
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
        , locationSpoils = [ ( 1 , Ammo ) , ( 1 , NewCard ) ]
        , locationType = [ LocWorker , LocAmmo ]
        , locationText =
            Storage
              [ ( Limit 2 , AnyOf [ Brick , Fuel , Gun , Iron ] )
              , ( Limit 2 , AnyOf [ Worker ] )
              ]
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
        , locationSpoils =
            [ ( 1 , ContactRed ) , ( 1 , Worker ) , ( 1 , NewCard ) ]
        , locationType = [ LocContact , LocGun ]
        , locationText =
            Convert [ ( 1 , AnyOf [ Worker ] ) ] [ ( 1 , ContactRed ) ] 2
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
        , locationSpoils = [ ( 2 , NewCard ) , ( 1 , VP ) ]
        , locationType = [ LocCard ]
        , locationText =
            Convert
              [ ( 1 , AnyOf [ Worker ] )
              , ( 1 , AnyOf [ Brick , Fuel , Gun , Iron ] )
              ]
              [ ( 2 , NewCard ) ]
              1
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
        , locationSpoils = [ ( 2 , Ammo ) ]
        , locationType = [ LocContact , LocAmmo ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "raze"
              , LexW2 "or"
              , LexW2 "make"
              , LexW2 "a"
              , LexW2 "deal"
              , LexW2 "gain"
              , LexR2 1 (Resource Ammo)
              ]
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
        , locationSpoils = [ ( 1 , Fuel ) , ( 1 , NewCard ) ]
        , locationType = [ LocFuel , LocAmmo ]
        , locationText =
            Storage [ ( Limit 3 , AnyOf [ Brick , Fuel , Gun , Iron ] ) ]
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
        , locationSpoils = [ ( 2 , Gun ) , ( 1 , NewCard ) ]
        , locationType = [ LocGun , LocVictory ]
        , locationText = Convert [ ( 1 , AnyOf [ Gun ] ) ] [ ( 1 , VP ) ] 2
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
        , locationSpoils = [ ( 3 , Shield ) ]
        , locationType = [ LocGun ]
        , locationText = Convert [] [ ( 1 , Shield ) ] 1
        , locationBuildBonus = Convert [] [ ( 1 , Shield ) ] 1
        , locationDeal = Shield
        , locationSet = Winter
        }
    )
  , ( 3
    , Location
        { locationName = "Wrecked Tank"
        , locationClass = Feature
        , locationDistance = 2
        , locationSpoils = [ ( 2 , VP ) ]
        , locationType = [ LocLiberty ]
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
        , locationSpoils = [ ( 2 , VP ) ]
        , locationType = [ LocLiberty ]
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
        , locationSpoils = [ ( 2 , ContactGrey ) ]
        , locationType = [ LocIron , LocContact ]
        , locationText = IfProducedAtLeast 1 (AnyOf [ Iron ]) 1 ContactGrey
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
        , locationSpoils = [ ( 1 , Ammo ) , ( 1 , Worker ) ]
        , locationType = [ LocToxic ]
        , locationText =
            Other
              [ LexW2 "spend"
              , LexR2 1 (Resource Worker)
              , LexW2 "to"
              , LexW2 "use"
              , LexW2 "a"
              , LexW2 "blocked"
              , LexW2 "open"
              , LexW2 "producation"
              , LexW2 "location"
              ]
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
        , locationSpoils = [ ( 2 , Ammo ) , ( 2 , VP ) ]
        , locationType = [ LocToxic , LocAmmo ]
        , locationText =
            Other
              [ LexW2 "each"
              , LexW2 "time"
              , LexW2 "you"
              , LexW2 "spend"
              , LexR2 2 (Resource Worker)
              , LexW2 "to"
              , LexW2 "gain"
              , LexW2 "a"
              , LexW2 "resource"
              , LexW2 "of"
              , LexW2 "the"
              , LexW2 "type"
              , LexW2 "produced"
              , LexW2 "by"
              , LexW2 "your"
              , LexW2 "faction"
              , LexW2 "board"
              , LexW2 "gain"
              , LexW2 "two"
              , LexW2 "of"
              , LexW2 "them"
              , LexW2 "instead"
              ]
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
        , locationSpoils = [ ( 1 , NewCard ) , ( 1 , VP ) ]
        , locationType = [ LocCard ]
        , locationText =
            Other
              [ LexW2 "discard"
              , LexR2 2 (Resource NewCard)
              , LexW2 "draw"
              , LexR2 2 (Resource NewCard)
              , LexW2 "keep"
              , LexW2 "one"
              , LexW2 "of"
              , LexW2 "them"
              , LexW2 "discard"
              , LexW2 "the"
              , LexW2 "other"
              ]
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
        , locationSpoils = [ ( 2 , VP ) ]
        , locationType = [ LocLiberty ]
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
        , locationSpoils = [ ( 2 , ContactRed ) , ( 1 , VP ) ]
        , locationType = [ LocGun ]
        , locationText =
            Other
              [ LexW2 "when"
              , LexW2 "you"
              , LexW2 "raze"
              , LexW2 "an"
              , LexW2 "opponents"
              , LexW2 "location"
              , LexW2 "it"
              , LexW2 "has"
              , LexW2 "a"
              , LexW2 "defense"
              , LexW2 "value"
              , LexW2 "of"
              , LexW2 "one"
              , LexW2 "less"
              , LexW2 "than"
              , LexW2 "normal"
              ]
        , locationBuildBonus = None
        , locationDeal = Worker
        , locationSet = PromoSet1
        }
    )
  ]
