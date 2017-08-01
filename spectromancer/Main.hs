{-# Language OverloadedStrings #-}
import Snap.Core(Snap,route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Control.Applicative ((<|>))

import Util.Snap(sendJSON)

import CardTypes(cardsToJSON)
import Cards(allCards)

main :: IO ()
main = quickHttpServe $
          route
            [ ("getCards", snapGetCards)
            , ("getState", snapGetState)
            ]
           <|> serveDirectory "ui"


snapGetCards :: Snap ()
snapGetCards = sendJSON (cardsToJSON allCards)

snapGetState :: Snap ()
snapGetState = return ()

