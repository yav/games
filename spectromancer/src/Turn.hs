{-# Language OverloadedStrings #-}
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

data GameInit = GameInit
    { rngSeed :: Int
    , firstPlayer, secondPlayer :: (Text, Class)
    } deriving Show

newGame :: GameInit -> Game
newGame gi =
  genRandFun (randSource (rngSeed gi)) $
    do let (name1, class1) = firstPlayer gi
           (name2, class2) = secondPlayer gi
       (deck1, deck2) <- pickDecks class1 class2
       p1 <- newPlayer name1 class1 deck1 Caster
       p2 <- newPlayer name2 class2 deck2 Opponent
       return $ \r -> activateCards . initialize $
                        Game { _curPlayer   = p1
                             , _otherPlayer = p2
                             , _leftPlayer  = Caster
                             , _gameRNG     = r  }

    where
      initialize g = 
        let (_, g', _) = runGame g startOfTurn in g'


newGameIO :: (Text,Class) -> (Text,Class) -> IO Game
newGameIO p1 p2 =
  do gen <- randSourceIO
     let (seed,_) = genRand gen randInt
     return $ newGame GameInit 
                      { rngSeed = seed
                      , firstPlayer = p1
                      , secondPlayer = p2 }

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
                  & leftPlayer  %~ theOtherOne
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
       Nothing -> stopError "No such card"
       Just c  -> do playCard c mbTgt
                     postTurn



