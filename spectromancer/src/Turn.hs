{-# Language OverloadedStrings, MultiWayIf #-}
module Turn(GameInit(..), newGame, turnSkip, turnPlayCard) where

import qualified Data.Map as Map
import Control.Lens( (^.), (.~), (%~), (&), ix, (^?) )
import Control.Monad(when, forM_)

import Game
import GameMonad
import Deck
import CardTypes
import Effects


-- | Setup a new game value.
newGame :: GameInit -> Game
newGame = activateCards . initialize . gameNew
  where
  initialize g = let (_, g', _) = runGame g setupGame in g'
  setupGame = do maybeSpawnGolem Caster
                 maybeSpawnGolem Opponent
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

-- | The common structure for player actions.
-- Keeps track of how many cards can be a played by a player.
turnAction :: GameM () -> GameM ()
turnAction a =
  do n <- withGame (playerCardNum Caster)
     if | n < 1     -> stopError "No more turns."
        | n == 1    -> do a
                          updGame_ (playerCardNum Caster %~ subtract 1)
                          postTurn
        | otherwise -> do a
                          updGame_ (playerCardNum Caster %~ subtract 1)

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
     n <- withGame (playerCardNum Caster)
     when (n <= 0) postTurn -- We don't get to do anything this turn: finish up


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
     maybeSpawnRabbit

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

    {-
  where
  curP = g ^. curPlayer

  activate ca = ca & deckCardEnabled .~ active (ca ^. deckCard)
    where
    have = Map.findWithDefault 0 (ca ^. deckCardElement) (curP ^. playerPower)
    active card = (card ^. cardCost) <= have && hasTarget (card ^. cardTarget)

    hasTarget tgt =
      case tgt of
        NoTarget -> True
        TargetCasterBlank -> Map.size curCreatures < slotNum
        TargetCaster's    -> not (Map.null curCreatures)
        TargetOpponent's  -> not (Map.null oppCreatures)
        TargetOpponent'sNormal ->
          any (\c -> c ^. deckCardElement /= Special) curCreatures
        TargetCreature ->
          not (Map.null curCreatures && Map.null oppCreatures)

  curCreatures = curP ^. playerActive
  oppCreatures = g    ^. player Opponent . playerActive
-}

