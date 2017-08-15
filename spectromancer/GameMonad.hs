{-# Language Rank2Types #-}
module GameMonad
  ( GameStatus(..)
  , GameStopped(..)
  , GameM

  , runGame
  , stopGame
  , getGame
  , setGame
  , updGame
  , updGame_
  , updPlayer_
  , withGame

  , getCreatureAt
  , getCreaturesFor

  , addLog
  , random

  ) where

import Control.Monad(ap,liftM)
import Control.Lens(Lens',(^.),(%~),(&),(.~))
import Data.Maybe(catMaybes)
import Util.Random(Gen,genRand)

import CardTypes
import Game

type Log   = [String] -> [String]

data GameStopped = GameWonBy Who
                 | IllegalMove DeckCard (Maybe Location)

data GameStatus a = GameStopped GameStopped
                  | GameOn a  -- ^ The game is in progress.

newtype GameM a   = GameM { unGameM :: Game -> (GameStatus a, Game, Log) }

instance Functor GameM where
  fmap = liftM

instance Applicative GameM where
  pure a = GameM (\g -> (GameOn a, g, id))
  (<*>)  = ap

instance Monad GameM where

  -- GameM a -> (a -> GameM b) -> GameM b
  m1 >>= f = GameM $ \g ->
    let (status, g1, out1) = unGameM m1 g
    in case status of
         GameOn a ->
            let (newStatus, g2, out2) = unGameM (f a) g1
            in (newStatus, g2, out1 . out2)
         GameStopped s -> (GameStopped s, g1, out1)


runGame :: Game -> GameM a -> (GameStatus a, Game, Log)
runGame g (GameM f) = f g

stopGame :: GameStopped -> GameM a
stopGame r = GameM (\g -> (GameStopped r, g, id))

getGame :: GameM Game
getGame = GameM (\g -> (GameOn g, g, id))

setGame :: Game -> GameM ()
setGame g = GameM (\_ -> (GameOn (), g, id))

addLog :: String -> GameM ()
addLog l = GameM (\g -> (GameOn (), g, (l :)))

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

-- | Do something random
random :: Gen a -> GameM a
random gen = updGame $ \g -> let (a,rnd1) = genRand (g ^. gameRNG) gen
                             in (a, g & gameRNG .~ rnd1)



withGame :: Lens' Game a -> GameM a
withGame l =
  do g <- getGame
     return (g ^. l)

getCreaturesFor :: Who -> GameM [(Location,DeckCard)]
getCreaturesFor w =
  do let slots = slotsFor w
     mbs <- mapM getCreatureAt (slotsFor w)
     return (catMaybes (zipWith addSlot slots mbs))
  where
  addSlot _ Nothing  = Nothing
  addSlot l (Just x) = Just (l,x)



updPlayer_ :: Who -> (Player -> Player) -> GameM ()
updPlayer_ w f = updGame_ (player w %~ f)

getCreatureAt :: Location -> GameM (Maybe DeckCard)
getCreatureAt l = withGame (creatureAt l)







