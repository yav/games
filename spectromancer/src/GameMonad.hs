{-# Language Rank2Types, OverloadedStrings #-}
module GameMonad
  ( GameStatus(..)
  , GameStopped(..)
  , GameM
  , Log, LogEvent(..)

  , runGame
  , stopGame
  , stopError
  , getGame
  , setGame
  , updGame
  , updGame_
  , updPlayer_
  , withGame

  , getCreatureAt
  , getCreaturesFor
  , findCreature
  , whenCreature

  , addLog
  , random

  ) where

import Control.Monad(ap,liftM)
import Control.Lens(Lens',(^.),(%~),(&),(.~))
import Data.Maybe(catMaybes)
import Util.Random(Gen,genRand)
import Data.Text(Text)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS

import CardTypes
import Deck
import Game

type Log = [LogEvent] -> [LogEvent]

data LogEvent = Say String
              | ChangeLife Location Int
              | ChangeWizardLife Who Int
              | CreatureDie Location
              | CreatureMove Location Location
              | CreatureAttack Location
              | SwapPlayers
              | CreatureSummon Location DeckCard
              | PowerChange Who Element Int
              | DoSomething Location
                deriving Show


data GameStopped = GameWonBy Who
                 | Err Text

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

addLog :: LogEvent -> GameM ()
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

findCreature :: Who -> Text -> GameM [(Location,DeckCard)]
findCreature w t = filter matches <$> getCreaturesFor w
  where matches (_,d) = deckCardName d == t




updPlayer_ :: Who -> (Player -> Player) -> GameM ()
updPlayer_ w f = updGame_ (player w %~ f)

getCreatureAt :: Location -> GameM (Maybe DeckCard)
getCreatureAt l = withGame (creatureAt l)

whenCreature :: Location -> (DeckCard -> GameM ()) -> GameM ()
whenCreature l k =
  do mb <- getCreatureAt l
     case mb of
       Nothing -> return ()
       Just d  -> k d

stopError :: Text -> GameM a
stopError t = stopGame (Err t)


--------------------------------------------------------------------------------

tag x = "tag" .= (x :: Text) 

instance ToJSON LogEvent where
  toJSON ev = JS.object $
    case ev of
      Say x -> [ tag "say", "text" .= x ]
      ChangeLife l n -> [ tag "life", "loc" .= l, "amount" .= n ]
      ChangeWizardLife w n -> [ tag "wizardLife", "who" .= w, "amount" .= n ]
      CreatureDie l -> [ tag "die", "loc" .= l ]
      CreatureAttack l -> [ tag "attack", "loc" .= l ]
      SwapPlayers -> [ tag "swap" ]
      CreatureSummon l d -> [ tag "summon", "loc" .= l, "card" .= d ]
      PowerChange w e n ->
        [ tag "power", "who" .= w, "element" .= e, "amount" .= n ]
      DoSomething l -> [ tag "doSomething", "loc" .= l ]
      CreatureMove l1 l2 -> [ tag "move", "from" .= l1, "to" .= l2 ]

instance ToJSON GameStopped where
 toJSON st = JS.object $ case st of
    GameWonBy who -> [ tag "finished"
                     , "winner" .= who ]
    Err txt       -> [ tag "error"
                     , "error" .= txt ]


instance ToJSON a => ToJSON (GameStatus a) where
  toJSON st = JS.object $ case st of
    GameStopped why -> [ tag "stopped"
                       , "why" .= why ]
    GameOn s -> [ tag "in_progress"
                , "data" .=  s ]
