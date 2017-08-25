module Turn where

import Data.Text(Text)
import Util.Random
import qualified Data.Map as Map
import Control.Lens( (^.), (.~), (%~), (&), ix, (^?) )

import Game
import GameMonad
import Deck
import CardTypes
import Effects

newGame :: StdGen -> (Text,Class) -> (Text,Class) -> Game
newGame rng (name1,class1) (name2,class2) =
  genRandFun rng $
    do (deck1, deck2) <- pickDecks class1 class2
       p1 <- newPlayer name1 deck1
       p2 <- newPlayer name2 deck2
       return $ \r -> activateCards
                        Game { _curPlayer   = p1
                             , _otherPlayer = p2
                             , _gameRNG     = r  }


newGameIO :: (Text,Class) -> (Text,Class) -> IO Game
newGameIO p1 p2 =
  do gen <- randSourceIO
     return (newGame gen p1 p2)


-- | Compute which cards in the deck are playable this turn and
-- disable opponents cards
activateCards :: Game -> Game
activateCards g =
  g & curPlayer   . eachCard . deckCardEnabled .~ True
    & otherPlayer . eachCard . deckCardEnabled .~ False
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
  oppCreatures = g    ^. otherPlayer . playerActive


postTurn :: GameM ()
postTurn =
  do creaturesAttack
     -- XXX: do end of turn cleanup (e.g., effects that
     -- only last for the current turn)
     endOfTurn
     g1 <- getGame
     setGame $ activateCards
             $ g1 & curPlayer   .~ g1 ^. otherPlayer
                 & otherPlayer .~ g1 ^. curPlayer
     addLog SwapPlayers
     generatePower
     -- XXX: enable creatures...
     startOfTurn
 

turnSkip :: GameM ()
turnSkip = postTurn
  

turnPlayCard :: Element -> Int -> Maybe Location -> GameM ()
turnPlayCard el cardNum mbTgt =
  do g <- getGame
     case g ^? player Caster . playerDeck . ix el . ix cardNum of
       Nothing -> stopGame (InvalidCard el cardNum)
       Just c  -> do playCard c mbTgt
                     postTurn



