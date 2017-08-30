{-# Language OverloadedStrings, RecordWildCards #-}
import Snap.Core(Snap,route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Control.Applicative ((<|>))
import Control.Monad(unless)
import Data.Aeson(toJSON, (.=))
import qualified Data.Aeson as JS
import Data.IORef(newIORef, readIORef, writeIORef)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map

import Util.Snap(sendJSON, snapIO, badInput, snapParam, snapParamSimpleEnum)

import GameMonad
import Game
import CardTypes(cardsToJSON,Location(..))
import CardIds
import Cards(allCards)
import Turn

data ServerState = ServerState
  { readState :: Snap Game
  , setState  :: Game -> Snap ()
  }

sendGame :: Game -> Log -> Snap ()
sendGame g f = sendJSON $ JS.object [ "game" .= g
                                    , "log"  .= f []
                                    ]

sendError :: Text -> Log -> Snap ()
sendError t l = badInput (Text.unwords (t : map (Text.pack . show) (l [])))

snapMsg m = snapIO (putStrLn m)

snapGameM :: ServerState -> GameM () -> Snap ()
snapGameM s m =
  do g <- readState s
     snapMsg "snapGameM begin"
     let (status, g1, output) = runGame g m
     case status of
       GameOn () -> do snapMsg "game on"
                       setState s g1
                       sendGame g1 output
       GameStopped why ->
         case why of
           GameWonBy w    -> do setState s g1
                                sendGame g1 output
           Err txt -> snapMsg "err" >> sendError txt output


newServerState :: Game -> IO ServerState
newServerState g =
  do r <- newIORef g
     return ServerState { readState = snapIO (readIORef r)
                        , setState  = \x -> snapIO (writeIORef r x)
                        }

main :: IO ()
main =
  do game <- newGameIO ("Player 1", goblins_cards)
                       ("Player 2", goblins_cards)

     s <- newServerState game

     quickHttpServe $
          route
            [ ("getCards", snapGetCards)
            , ("getState", snapGetState s)
            , ("newGame",  snapNewGame s)
            , ("playCard", snapPlayCard s)
            , ("playTargetedCard", snapPlayTargetedCard s)
            , ("skipTurn", snapSkipTurn s)
            ]
           <|> serveDirectory "ui"


snapGetCards :: Snap ()
snapGetCards = sendJSON (cardsToJSON allCards)

snapGetState :: ServerState -> Snap ()
snapGetState s =
  do game <- readState s
     sendGame game id

snapNewGame :: ServerState -> Snap ()
snapNewGame self =
  do c1 <- snapCardClass "player1"
     c2 <- snapCardClass "player2"
     game <- snapIO $ newGameIO ("Player 3", c1) ("Player 4", c2)
     setState self game
     sendGame game id

snapPlayCard :: ServerState -> Snap ()
snapPlayCard self =
  do e  <- snapParamSimpleEnum "element"
     c  <- snapParam "card"
     snapGameM self (turnPlayCard e c Nothing)

snapSkipTurn :: ServerState -> Snap ()
snapSkipTurn self =
  do snapGameM self (turnSkip)

snapPlayTargetedCard :: ServerState -> Snap ()
snapPlayTargetedCard s =
  do e <- snapParamSimpleEnum "element"
     c <- snapParam "card"
     l <- snapParam "loc"
     w <- snapParamSimpleEnum "who"
     snapGameM s (turnPlayCard e c (Just Location { locWho = w, locWhich = l }))

snapCardClass :: Text -> Snap Text
snapCardClass pname =
  do e <- snapParam pname
     unless (e `Map.member` allCards) $
       sendError ("Invalid class in param: " `Text.append` pname) id
     return e



