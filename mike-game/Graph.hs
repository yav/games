{-# Language GeneralizedNewtypeDeriving, RecordWildCards #-}
import MonadLib
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

data S = S
  { rooms       :: !(Map Int Room)
  , nextRoom    :: !Int
  , connections :: !(Set (Int,Int,Color))
  }

newtype Description a = D (StateT S Lift a)
  deriving (Functor,Applicative,Monad)

newtype RoomId = RoomId Int

room :: String -> Description RoomId
room x = D $ sets $ \S { .. } ->
  let r = Room { roomName = x }
      s = S { rooms    = Map.insert nextRoom r rooms
            , nextRoom = nextRoom + 1
            , ..
            }
  in r `seq` s `seq` (RoomdId nextRoom, s)

connection :: RoomId -> RoomId ->   



data Color = Blue | Red | Green | Purple
  deriving (Show,Eq,Ord)

data Room = Room { roomName :: !String }



