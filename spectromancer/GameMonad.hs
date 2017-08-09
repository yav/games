module GameMonad
  ( GameStatus(..)
  , GameM

  , runGame
  , winGame
  , getGame
  , setGame
  , updGame
  , updGame_
  , updPlayer_
  , withGame

  , getCreatureAt
  , getCreaturesFor

  , addLog

  ) where

import Control.Monad(ap,liftM)
import Data.Maybe(catMaybes)

import CardTypes
import State

type Log = [String] -> [String]

data GameStatus a = GameWonBy Who Game Log
                    -- ^ Game state and log as it was when the game ended
                  | GameOn a Game Log  -- ^ The current game state and log

newtype GameM a   = GameM { unGameM :: Game -> GameStatus a }

instance Functor GameM where
  fmap = liftM

instance Applicative GameM where
  pure a = GameM (\g -> GameOn a g id)
  (<*>)  = ap

instance Monad GameM where

  -- GameM a -> (a -> GameM b) -> GameM b
  GameM m1 >>= f = GameM $ \g ->
    case m1 g of
      GameWonBy w g' l -> GameWonBy w g' l
      GameOn a  g1 ls  -> addLogStatus ls (unGameM (f a) g1)

addLogStatus :: Log -> GameStatus a -> GameStatus a
addLogStatus xs s =
  case s of
    GameWonBy w g ys -> GameWonBy w g (xs . ys)
    GameOn a g ys    -> GameOn a g (xs . ys)

runGame :: Game -> GameM a -> GameStatus a
runGame g (GameM f) = f g

winGame :: Who -> GameM a
winGame w = GameM (\g -> GameWonBy w g id)

getGame :: GameM Game
getGame = GameM (\g -> GameOn g g id)

setGame :: Game -> GameM ()
setGame g = GameM (\_ -> GameOn () g id)

addLog :: String -> GameM ()
addLog l = GameM (\g -> GameOn () g (l :))

--------------------------------------------------------------------------------

updGame_ :: (Game -> Game) -> GameM ()
updGame_ f =
  do g <- getGame
     setGame (f g)

updGame :: (Game -> (a,Game)) -> GameM a
updGame f =
  do g <- getGame
     let (a,g1) = f g
     setGame g1
     return a

withGame :: (Game -> a) -> GameM a
withGame f =
  do g <- getGame
     return (f g)

getCreatureAt :: Location -> GameM (Maybe DeckCard)
getCreatureAt l = withGame (`creatureAt` l)

getCreaturesFor :: Who -> GameM [(Location,DeckCard)]
getCreaturesFor w =
  do let slots = slotsFor w
     mbs <- mapM getCreatureAt (slotsFor w)
     return (catMaybes (zipWith addSlot slots mbs))
  where
  addSlot _ Nothing  = Nothing
  addSlot l (Just x) = Just (l,x)



updPlayer_ :: Who -> (Player -> Player) -> GameM ()
updPlayer_ w f = updGame_ (gameUpdatePlayer w f)





