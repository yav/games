{-# Language RecordWildCards, ViewPatterns #-}
import qualified Types as Types
import Types
import Cards
import Text.Show.Pretty(ppShow)
import Text.Read(readMaybe)
import Data.List
import Data.Char

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

  | Act [ (Int,CostReward) ] Action Limit
    -- ^ Pay the cost to do the actions.
    -- The limit indicates how many times can you use this.

  | SecretStock
    -- ^ each time you spend 2 workers to gain a resource of the type
    -- produced by your faction board gain two of them instead

  | DecreaseDefence Int

  | None

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


main :: IO ()
main = writeFile "NewCards.hs" $ unlines $
          [ "{-# Langauge OverloadedStrings #-}"
          , "module Cards where"
          , ""
          , "import Util.Bag"
          , "import qualified Data.Set as Set
          , "import Types"
          , ""
          , "connections :: [(Int,Connection)]"
          , "connections ="
          ] ++ indent (ppShow (map convertConn connections)) ++
          [ ""
          , ""
          , "locations :: [(Int,Location)]"
          , "locations ="
          ] ++ indent (ppShow (map convertLoc locations))

  where indent = map ("  " ++) . lines

convertConn :: (Int, Types.Connection) -> (Int, Connection)
convertConn (x,c) = (x,c1)
  where
  c1 = Connection
         { connectionName = Types.connectionName c
         , connectionText = convertDecs $ Types.connectionText c
         , connectionSet  = Types.connectionSet c
         }


convertLoc :: (Int,Types.Location) -> (Int,Location)
convertLoc (x,s) = (x,s1)
  where
  s1 = Location
         { locationName       = Types.locationName s
         , locationClass      = Types.locationClass s
         , locationDistance   = Types.locationDistance s
         , locationSpoils     = Types.locationSpoils s
         , locationType       = Types.locationType s
         , locationText       = convertDecs $ Types.locationText s
         , locationBuildBonus = convertDecs $ Types.locationBuildBonus s
         , locationDeal       = Types.locationDeal s
         , locationSet        = Types.locationSet s
         }

convertDecs :: Types.Description -> Description
convertDecs desc =
  case desc of

    Types.OneTimeCache [(Int,Resource)]
    Types.ForEachLoc LocType Int Resource Limit
    Types.ForEachProduced Resource Int Resource Limit
    Types.EachTime Event Int Resource
    Types.Storage [ (Limit,Resource') ]
    Types.Do Action
    Types.Act [ (Int,Resource') ] Action Limit
    Types.SecretStock -> SecretStock
    Types.DecreaseDefence n -> DecreaseDefence n
    Types.None -> None









