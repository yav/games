{-# Language OverloadedStrings #-}
import Snap.Core(Snap,route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent(MVar,newMVar,readMVar,modifyMVar_)
import Data.Aeson(toJSON)

import Util.Snap(sendJSON)

import CardTypes(cardsToJSON)
import Cards(allCards)
import State



type SnapState = MVar Game

main :: IO ()
main =
  do game <- newGameIO ("Player 1", "VAMPIRIC CARDS")
                       ("Player 2", "SORCERY CARDS")

     state <- newMVar game

     quickHttpServe $
          route
            [ ("getCards", snapGetCards)
            , ("getState", snapGetState state)
            , ("newGame",  snapNewGame state)
            ]
           <|> serveDirectory "ui"


snapGetCards :: Snap ()
snapGetCards = sendJSON (cardsToJSON allCards)

snapGetState :: SnapState -> Snap ()
snapGetState s =
  do game <- liftIO (readMVar s)
     sendJSON (toJSON game)

snapNewGame :: SnapState -> Snap ()
snapNewGame s =
  do game <- liftIO $
      do game <- newGameIO ("Player 3", "VAMPIRIC CARDS")
                           ("Player 4", "SORCERY CARDS")
         modifyMVar_ s $ \_ -> return game
         return game
     sendJSON (toJSON game)
