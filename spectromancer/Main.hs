{-# Language OverloadedStrings #-}
import Snap.Core(Snap,route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Control.Applicative ((<|>))

import Util.Snap(sendJSON)

import Types(cardsToJSON)
import Cards(allCards)

main :: IO ()
main = quickHttpServe $
          route
            [ ("getCards", snapGetCards)
            ]
           <|> serveDirectory "ui"


snapGetCards :: Snap ()
snapGetCards = sendJSON (cardsToJSON allCards)

