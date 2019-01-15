{-# Language TypeOperators #-}
module Game where

import Data.Map(Map)
import qualified Data.Map as Map
import Util.Bag
import Control.Monad(guard)
import Data.Maybe(fromMaybe)

data Dir = N | E | S | W
  deriving (Eq,Ord,Show)

newtype MeepType = MeepType Int
  deriving (Eq,Ord)

type a |-> b = Map a b
type Loc     = (Int,Int)

data Game = Game
  { terrain   :: Loc -> MeepType -> Maybe Int  -- ^ Terrain costs for meeples
  , width, height :: Int
  , meeples   :: MeepType |-> Loc          -- ^ Maps location to meeples on it
  , controls  :: Dir   |-> Bag MeepType    -- ^ The state of the controls
  , status    :: Status
  , steps     :: Int
  }

data Status = Activate | Distribute Dir (Bag MeepType) | Finished

nextLoc :: Dir -> Loc -> Loc
nextLoc d (x,y) =
  case d of
    N -> (x,y-1)
    E -> (x+1,y)
    S -> (x,y+1)
    W -> (x-1,y)

nextDir :: Dir -> Dir
nextDir d =
  case d of
    N -> E
    E -> S
    S -> W
    W -> N

newGame :: Game
newGame = Game
  { terrain = \(x,y) _ -> do guard (x >= 0 && x < 6 && y >= 0 && y < 6)
                             pure 1
  , meeples = Map.fromList [ (t1, (0,5)), (t2, (5,0)) ]
  , width = 6
  , height = 6
  , controls = Map.fromList [ (N, bagFromList [t1])
                            , (E, bagFromList [t1,t2])
                            , (S, bagFromList [t2])
                            , (W, bagFromList [t1,t2])
                            ]
  , status = Activate
  , steps  = 0
  }
  where
  t1 = MeepType 1
  t2 = MeepType 3

-- | Drop a meeple of the goven color at the next location.
distribute :: MeepType -> Game -> Game
distribute c g =
  case status g of
    Distribute d todo ->
      case bagRemove 1 c todo of
        Nothing -> g
        Just newTodo
          | bagIsEmpty newTodo -> g1 { status = Activate }
          | otherwise          -> autoDist
                                  g1 { status = Distribute (nextDir d) newTodo }
            where g1 = g { controls = Map.adjust (bagAdd 1 c) d (controls g) }
    _ -> g

-- | Move a meeple on the terrain
activate :: Dir -> Game -> Game
activate d g =
  case status g of
    Activate -> newG
    _        -> g
  where
  newG = checkGameEnd
       $ autoDist
         g { meeples  = newMp
           , controls = Map.insert d newCs (controls g)
           , status   = if bagIsEmpty toDistr
                          then Activate
                          else Distribute (nextDir d) toDistr
           , steps    = steps g + 1
           }
  todo = bagToListGrouped (Map.findWithDefault bagEmpty d (controls g))
  (toDistr,newCs,newMp) = moveMeeples todo (meeples g)

  moveMeeples cs mp =
    case cs of
      [] -> (bagEmpty, bagEmpty, mp)
      (c,n) : more ->
        let (n1,mp1) = moveMeeple c (n,mp)
            (used,unused,mp2) = moveMeeples more mp1
        in (bagAdd (n-n1) c used, bagAdd n1 c unused, mp2)

  moveMeeple c (amt,mp) =
    fromMaybe (amt,mp) $
    do loc <- Map.lookup c mp
       let newLoc = nextLoc d loc
       cost <- terrain g newLoc c
       guard (amt >= cost)
       pure (moveMeeple c (amt-1, Map.insert c newLoc mp))

checkGameEnd :: Game -> Game
checkGameEnd g
  | same (Map.elems (meeples g)) = g { status = Finished }
  | otherwise = g

  where
  same xs = case xs of
              [] -> True
              x : rest -> all (x ==) rest

autoDist :: Game -> Game
autoDist g =
  case status g of
    Distribute i xs
      | [(a,_)] <- bagToListGrouped xs -> distribute a g
    _ -> g
