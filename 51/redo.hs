{-# Language RecordWildCards, ViewPatterns #-}
import qualified Types as Types
import Types
import Cards
import Text.Show.Pretty(ppShow)
import Text.Read(readMaybe)
import Data.List
import Data.Char

main :: IO ()
main = writeFile "NewCards.hs" $ unlines $
          [ "module Cards where"
          , ""
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
convertDecs (Act [] a (Limit 1)) = Do a

convertDecs x = x






