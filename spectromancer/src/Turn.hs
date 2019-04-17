{-# Language OverloadedStrings, MultiWayIf #-}
module Turn(GameInit(..), newGame, turnSkip, turnPlayCard) where

import qualified Data.Map as Map
import Control.Lens( (^.), (.~), (%~), (&), ix, (^?), view )
import Control.Monad(when, forM_)

import Game
import GameMonad
import Deck
import CardTypes
import Effects

import qualified Decks.Forest
import qualified Decks.Golem


-- | Setup a new game value.
newGame :: GameInit -> Game
newGame = activateCards . initialize . gameNew
  where
  initialize g = let (_, g', _) = runGame g setupGame in g'
  setupGame = do _ <- Decks.Golem.maybeSpawnGolem Caster
                 _ <- Decks.Golem.maybeSpawnGolem Opponent
                 startOfTurn


-- | The player chose to not play a card.
turnSkip :: GameM ()
turnSkip = turnAction (return ())

-- | The player chose to a play a card.
turnPlayCard :: Element -> Int -> Maybe Location -> GameM ()
turnPlayCard el cardNum mbTgt =
  do g <- getGame
     case g ^? player Caster . playerDeck . ix el . ix cardNum of
       Nothing -> stopError "No such card"
       Just c  -> turnAction (playCard c mbTgt)


--------------------------------------------------------------------------------


-- | The common structure for player actions.
-- Keeps track of how many cards can be a played by a player.
turnAction :: GameM () -> GameM ()
turnAction a =
  do n <- withGame (view (playerCardNum Caster))
     if | n < 1     -> stopError "No more turns."
        | otherwise -> do a
                          updGame_ (playerCardNum Caster %~ subtract 1)
                          checkEndOfTurn

-- | Check to see if the curren the current player is allowed to play
-- any more cards.
checkEndOfTurn :: GameM ()
checkEndOfTurn =
  do n <- withGame (view (playerCardNum Caster))
     when (n <= 0) postTurn

-- | This is what happens at the end of a player's turn---when they've
-- played all the cards they could play.
postTurn :: GameM ()
postTurn =
  do mapM_ creaturePerformAttack (slotsFor Caster)
     endOfTurn
     swapPlayers
     newTurn

-- | This is how a typical turn goes.
newTurn :: GameM ()
newTurn =
  do generatePower
     updGame_ (playerCardNum Caster %~ (+1)) -- you get to play 1 card
     startOfTurn
     checkEndOfTurn


-- | Now it is the other player's turn.
swapPlayers :: GameM ()
swapPlayers =
  do updGame_ $ \g1 -> activateCards $
                       g1 & player Caster   .~ g1 ^. player Opponent
                          & player Opponent .~ g1 ^. player Caster
                          & firstPlayerStart %~ theOtherOne
     addLog SwapPlayers



-- | Power generation at the start of a player's turn.
generatePower :: GameM ()
generatePower =
  do ours   <- map snd <$> getCreaturesFor Caster
     theirs <- map snd <$> getCreaturesFor Opponent

     let baseGrowth  = zip allElements (repeat 1)
         ourEffects  = concatMap (creatureModifyPowerGrowth Caster) ours
         theirEffect = concatMap (creatureModifyPowerGrowth Opponent) theirs
         changes     = Map.fromListWith (+)
                          (baseGrowth ++ ourEffects ++ theirEffect)

     forM_ (Map.toList changes) $ \(el,n) -> wizChangePower Caster el n

-- | Special effects that happen at the beginning of a turn.
startOfTurn :: GameM ()
startOfTurn =
  do forM_ (slotsFor Caster) creatureStartOfTurn
     Decks.Forest.maybeSpawnRabbit

-- | Special effects that happen at the end of a turn.
endOfTurn :: GameM ()
endOfTurn =
  do mapM_ creatureEndOfTurn (slotsFor Caster)
     wizUpd_ Caster (creatures %~ deckCardAgeMods)






-- | Compute which cards in the deck are playable this turn and
-- disable opponents cards
-- XXX: we can be smarter here by doing a bit of simulation to look ahead.
activateCards :: Game -> Game
activateCards g =
  g & player Caster   . eachCard . deckCardEnabled .~ True
    & player Opponent . eachCard . deckCardEnabled .~ False

