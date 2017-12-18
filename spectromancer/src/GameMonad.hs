{-# Language Rank2Types, OverloadedStrings, MultiWayIf #-}
module GameMonad
  ( runGame
  , GameM
  , GameStatus(..)
  , GameStopped(..)

   -- * Logging
  , addLog
  , doSomething
  , Log, LogEvent(..)

   -- * Interruptions
  , stopError
  , checkGameWon

    -- * Acces to game
  , getGame
  , setGame
  , updGame
  , updGame_
  , withGame

  -- * Modify a player
  , wizUpd_
  , wizChangeLife
  , wizChangePower

  -- * The Battlefield
  , countLiving

  -- ** Summoning
  , summonCreature
  , summonLR

  -- ** Access to creatures
  , getCreatureAt
  , getCreaturesFor
  , findCreature
  , whenCreature

  -- ** React
  , creatureReact


  -- ** Modifying summoned creatures
  , creatureChangeLife
  , creatureChangeLife_
  , creatureChangeAttack
  , creatureMove
  , creatureSkipNextAttack

  , creatureMod
  , creatureRmMod
  , creatureGetMods
  , creatureTemporaryAttackBoost

  -- * Randomness
  , randomBlankSlot
  , randomPower
  , randomCreature
  , random
  ) where

import Control.Monad(ap,liftM,unless,when)
import Control.Lens((^.),(%~),(&),(.~),mapped,view)
import Data.Maybe(catMaybes)
import Data.List(partition)
import Util.Random(Gen,genRand)
import Data.Text(Text)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS
import qualified Data.Map as Map

import Util.Random(oneOf)

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


data GameStopped = GameWonBy Who Game
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


getGame :: GameM Game
getGame = GameM (\g -> (GameOn g, g, id))

setGame :: Game -> GameM ()
setGame g = GameM (\_ -> (GameOn (), g, id))

addLog :: LogEvent -> GameM ()
addLog l = GameM (\g -> (GameOn (), g, (l :)))

--------------------------------------------------------------------------------


doSomething :: Location -> GameM ()
doSomething l = addLog (DoSomething l)


--------------------------------------------------------------------------------
-- Modifications and acces to the game

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
withGame f = f <$> getGame


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Stop game

stopError :: Text -> GameM a
stopError t = GameM (\g -> (GameStopped (Err t), g, id))

stopWon :: Who -> GameM a
stopWon w = GameM (\g -> (GameStopped (GameWonBy w g), g, id))

-- | Is this a finished game?
checkGameWon :: GameM ()
checkGameWon =
  do g <- getGame
     if | g ^. player Opponent . playerLife <= 0 -> stopWon Caster
        | g ^. player Caster . playerLife <= 0   -> stopWon Opponent
        | otherwise                          -> return ()
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- Getting creatures from battlefield

-- | Get all creatures for one of the players.
getCreaturesFor :: Who -> GameM [(Location,DeckCard)]
getCreaturesFor w =
  do let slots = slotsFor w
     mbs <- mapM getCreatureAt (slotsFor w)
     return (catMaybes (zipWith addSlot slots mbs))
  where
  addSlot _ Nothing  = Nothing
  addSlot l (Just x) = Just (l,x)

-- | Find the creatures with the given name.
findCreature :: Who -> Text -> GameM [(Location,DeckCard)]
findCreature w t = filter matches <$> getCreaturesFor w
  where matches (_,d) = deckCardName d == t

-- | Get the creature at the given location.
getCreatureAt :: Location -> GameM (Maybe DeckCard)
getCreatureAt l = withGame (view (creatureAt l))

-- | Execute the action if the given location is occupied.
whenCreature :: Location -> (DeckCard -> GameM ()) -> GameM ()
whenCreature l k =
  do mb <- getCreatureAt l
     case mb of
       Nothing -> return ()
       Just d  -> k d
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Summoning

-- | Place the given card on the location.
summonCreature :: DeckCard -> Location -> GameM()
summonCreature dc l =
  do updGame_ (creatureAt l .~ Just dc)
     addLog $ CreatureSummon l dc


-- | Place the given card in the slots neighbouring the locaton.
summonLR :: Location -> DeckCard -> GameM ()
summonLR ctr smn =
  do let place l' = when (onBoard l') $
                    do mb <- getCreatureAt l'
                       case mb of
                          Nothing -> summonCreature smn l'
                          Just _  -> return ()
     place (leftOf ctr)
     place (rightOf ctr)
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------


-- | Modify the creature temporarily.
creatureMod :: Location -> DeckCardMod -> ModDuration -> GameM ()
creatureMod l x y =
  updGame_ $ creatureAt l . mapped %~ deckCardAddMod (x,y)

-- | Remove mods that mathch the preidcate.
creatureRmMod :: Location -> ((DeckCardMod,ModDuration) -> Bool) -> GameM ()
creatureRmMod l p = updGame_ $ creatureAt l . mapped %~ deckCardRmMods p

creatureGetMods :: Location -> GameM [(DeckCardMod, ModDuration)]
creatureGetMods l =
  do mb <- getCreatureAt l
     return (maybe [] (\x -> x ^. deckCardMods) mb)

creatureTemporaryAttackBoost :: Location -> Int -> GameM ()
creatureTemporaryAttackBoost l amt =
  creatureMod l (AttackBoost amt) (UntilEndOfTurn 0)



-- | Modify the attack value of a creature.
creatureChangeAttack :: Location -> Int -> GameM ()
creatureChangeAttack l amt =
  whenCreature l $ \c ->
     do let c1 = c & atk %~ (+amt)
            atk = deckCard . creatureCard . creatureAttack . mapped
        updGame_ $ creatureAt l .~ Just c1

-- | Move a creature from one location to another.
-- Fails of the source location does not have a creature, or
-- the target location is occupied, or outside the board.
creatureMove :: Location -> Location -> GameM ()
creatureMove lFrom lTo =
  do mbC <- getCreatureAt lFrom
     c   <- case mbC of
              Nothing -> stopError "Nothing to move"
              Just c  -> return c
     mbTGT <- getCreatureAt lTo
     case mbTGT of
       Nothing -> return ()
       Just _ -> stopError "Cannot move on top of other creatures."
     unless (onBoard lTo) $
       stopError "The creature cannot move outside the board"
     addLog (CreatureMove lFrom lTo)
     updGame_ ( (creatureAt lFrom .~ Nothing)
              . (creatureAt lTo   .~ Just c)
              )


-- | Adjust the life of a creature.
creatureChangeLife_ :: Location -> Int -> GameM ()
creatureChangeLife_ l n = creatureChangeLife l n >> return ()


-- | Change the life of a creature.  Returns the actual change in life
creatureChangeLife :: Location -> Int -> GameM Int
creatureChangeLife l n =
  do mb <- getCreatureAt l
     case mb of
       Nothing -> return 0
       Just d ->
         do let maxLife = d ^. deckCardOrig . creatureCard . creatureLife
                curLife = d ^. deckCardLife
                newLife = max 0 (min (curLife + n) maxLife)
                change  = newLife - curLife
            unless (change == 0) $
             do updGame_ (creatureAt l .~ Just (d & deckCardLife .~ newLife))
                addLog (ChangeLife l change)
            return change


-- | Creature does not attack this turn.
creatureSkipNextAttack :: Location -> GameM ()
creatureSkipNextAttack l =
  updGame_ (creatureAt l . mapped %~ deckCardAddMod (SkipNextAttack,UntilEndOfTurn 1))


-- | Common pattern for creature reactions.
creatureReact ::
  [(Text, (Location,DeckCard) -> Location -> GameM ())]
                                              {- ^ Special abilities -} ->
  Location {- ^ Creature that is reacting -} ->
  Location {- ^ Location of the arget causing the reaction -} ->
  GameM ()
creatureReact ab = \cl tgtl ->
  do mb <- getCreatureAt cl
     case mb of
       Nothing -> return ()
       Just c ->
         case Map.lookup (deckCardName c) abilities of
           Nothing  -> return ()
           Just act -> do doSomething cl
                          act (cl,c) tgtl
  where
  abilities = Map.fromList ab



-- | Count how many of the given slots contain living things, and
-- how many contain dead things.
countLiving :: [Location] -> GameM (Int,Int) -- ^ How many lived, and died
countLiving ls =
  do g <- getGame
     let (deads,alives) = partition (\(_,dc) -> (dc ^. deckCardLife) <= 0)
                                   (inhabitedSlots g ls)
     return (length alives, length deads)



--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- | Update one of the players.
wizUpd_ :: Who -> (Player -> Player) -> GameM ()
wizUpd_ w f = updGame_ (player w %~ f)


-- | Change the player's life.
wizChangeLife :: Who -> Int -> GameM ()
wizChangeLife w a =
  do updGame_ (player w . playerLife %~ (+a))
     addLog (ChangeWizardLife w a)


-- | Change the player's power in the given element.
wizChangePower :: Who -> Element -> Int -> GameM ()
wizChangePower w e i =
  do wizUpd_ w (playerPower e %~ upd)
     addLog (PowerChange w e i)
  where upd x = max 0 (x + i)
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------

-- | Do something random
random :: Gen a -> GameM a
random gen = updGame $ \g -> let (a,rnd1) = genRand (g ^. gameRNG) gen
                             in (a, g & gameRNG .~ rnd1)


-- | Select a random blank location, if any.
randomBlankSlot :: Who -> GameM (Maybe Location)
randomBlankSlot who =
  do as <- withGame (view (player who . playerActive))
     let free = [ s | s <- take slotNum [ 0 .. ], not (s `Map.member` as) ]
     case free of
       [] -> return Nothing
       _  -> do s <- random (oneOf free)
                return (Just Location { locWho = who, locWhich = s })


-- | Select a random creature for one of the players, if any.
randomCreature :: Who -> GameM (Maybe Location)
randomCreature who =
  do as <- withGame (view (player who . playerActive))
     case Map.keys as of
       [] -> return Nothing
       cs -> do s <- random (oneOf cs)
                return (Just Location { locWho = who, locWhich = s })


-- | Select a random power element.
randomPower :: GameM Element
randomPower = random (oneOf allElements)
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Export to JS


tag :: JS.KeyValue kv => Text -> kv
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
    GameWonBy who _ -> [ tag "finished"
                       , "winner" .= who ]
    Err txt         -> [ tag "error"
                       , "error" .= txt ]


instance ToJSON a => ToJSON (GameStatus a) where
  toJSON st = JS.object $ case st of
    GameStopped why -> [ tag "stopped"
                       , "why" .= why ]
    GameOn s -> [ tag "in_progress"
                , "data" .=  s ]
