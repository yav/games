{-# Language OverloadedStrings, RecordWildCards #-}
import Snap.Core(Snap,route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Control.Applicative ((<|>))
import Data.Aeson(toJSON)
import Data.IORef(newIORef, readIORef, atomicModifyIORef')
import Data.Text(Text)

import Util.Snap(sendJSON, snapIO, badInput, snapParam, snapParamSimpleEnum)

import CardTypes(cardsToJSON)
import CardIds
import Cards(allCards)
import State

data ServerState = ServerState
  { readState :: Snap Game
  , modifyState :: (Game -> Either Text Game) -> Snap Game
  }

newServerState :: Game -> IO ServerState
newServerState g =
  do s <- newIORef g
     let readState = snapIO $ readIORef s
         modifyState f =
           do r <- snapIO $ atomicModifyIORef' s $ \g ->
                            case f g of
                              Left err -> (g,Left err)
                              Right g1 -> (g1, Right g1)
              case r of
                Left err -> badInput err
                Right g  -> return g
     return ServerState { .. }

main :: IO ()
main =
  do game <- newGameIO ("Player 1", vampiric_cards)
                       ("Player 2", sorcery_cards)

     s <- newServerState game

     quickHttpServe $
          route
            [ ("getCards", snapGetCards)
            , ("getState", snapGetState s)
            , ("newGame",  snapNewGame s)
            , ("playCard", snapPlayCard s)
            ]
           <|> serveDirectory "ui"


snapGetCards :: Snap ()
snapGetCards = sendJSON (cardsToJSON allCards)

snapGetState :: ServerState -> Snap ()
snapGetState s =
  do game <- readState s
     sendJSON (toJSON game)

snapNewGame :: ServerState -> Snap ()
snapNewGame s =
  do game <- snapIO $ newGameIO ("Player 3", vampiric_cards)
                                ("Player 4", sorcery_cards)
     _ <- modifyState s $ \_ -> Right game
     sendJSON (toJSON game)

snapPlayCard :: ServerState -> Snap ()
snapPlayCard s =
  do e <- snapParamSimpleEnum "element"
     c <- snapParam "card"
     l <- snapParam "loc" -- snapOptParam "loc"
     g1 <- modifyState s $ playCard e c l
     sendJSON (toJSON g1)





