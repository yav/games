{-# LANGUAGE OverloadedStrings #-}
import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory)
import           Control.Applicative ((<|>))
import           Control.Monad((<=<))
import           Control.Concurrent

import Util.Snap
import Util.Random
import Util.History
import Util.Perhaps

import Hyperborea.Types
import Hyperborea.Rules
import Hyperborea.Actions
import Hyperborea.Protocol

data GameState = GameState { theGame :: History Factory }
type S = MVar GameState

test :: Gen Factory
test =
  do f <- factoryEmpty
     let f1 = factoryAddGroup g f
         f2 = factoryAddGroup g f1
     return $ factoryRestock
            $ factoryExtendSource (Raw Green)
            $ factoryExtendSource (Raw Red)
            $ factoryExtendSource (Raw Magenta)
            $ factoryExtendSource (Raw Magenta)
            $ factoryExtendSource (Raw Magenta)
            $ factoryExtendSource (Raw Magenta)
            $ factoryChangeSize 3 f2





  where
  g = basic !! 0



main :: IO ()
main =
  do r <- randSourceIO
     let (fact,_) = genRand r test
     s <- newMVar GameState { theGame = history fact }
     quickHttpServe $ Snap.route
       [ ("/view",      snapView s)

       , ("/apply",     snapApply s)
       , ("/produce",   snapProduce s)

       , ("/setReset",  snapSetReset s)

       , ("/endPeriod", snapEndPeriod s)
       , ("/restock",   snapRestock s)
       , ("/reset",     snapReset s)

       , ("/use",   snapUse s)
       ] <|> serveDirectory "ui"

snapView :: S -> Snap ()
snapView s =
  do h <- snapIO (readMVar s)
     sendJSON (toJS (historyCurrent (theGame h)))

snapWithGame :: S -> (Factory -> Perhaps Factory) -> Snap ()
snapWithGame s upd =
  snapModifyMVar_ s $ \state ->
    case historyUpdateF upd (theGame state) of
      Ok h -> do let s1 = state { theGame = h }
                 sendJSON (toJS (historyCurrent h))
                 return s1
      Failed x -> badInput x

snapApply :: S -> Snap ()
snapApply s =
  do g <- snapParam "group"
     r <- snapParam "rule"
     m <- snapParam "material"
     snapWithGame s (factoryUseMaterial m g <=< factoryActivate r g)

snapUse :: S -> Snap ()
snapUse s =
  do n <- snapParam "amount"
     a <- snapParamSimpleEnum "action"
     snapWithGame s (factoryUse n a)

snapProduce :: S -> Snap ()
snapProduce s =
  do g <- snapParam "group"
     v <- snapParam "variant"
     snapWithGame s (factoryProduce v g)

snapEndPeriod :: S -> Snap ()
snapEndPeriod s = snapWithGame s (return . factoryEndPeriod)

snapRestock :: S -> Snap ()
snapRestock s = snapWithGame s (return . factoryRestock)

snapReset :: S -> Snap ()
snapReset s = snapWithGame s (return . factoryReset)



snapSetReset :: S -> Snap ()
snapSetReset s =
  do g <- snapParam "group"
     v <- snapParam "value"
     snapWithGame s (factoryForceReset v g)

