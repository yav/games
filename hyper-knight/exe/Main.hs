{-# LANGUAGE OverloadedStrings #-}
import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory)
import           Control.Applicative ((<|>))
import           Control.Monad((<=<))
import           Control.Concurrent

import Util.Bag
import Util.Snap
import Util.Random
import Util.History
import Util.Perhaps

import HyperKnight.Rules

data GameState = GameState { theGame :: History Factory }
type S = MVar GameState

test :: Gen Factory
test =
  do f <- factoryEmpty
     let Ok f1 = factoryAddGroup g f
         Ok f2 = factoryAddGroup g f1
     return $ factoryRestock
            $ factoryExtendSource (Raw A)
            $ factoryExtendSource (Raw B)
            $ factoryExtendSource (Raw C)
            $ factoryExtendSource (Raw C)
            $ factoryExtendSource (Raw C)
            $ factoryExtendSource (Raw C)
            $ factoryChangeSize 3 f2





  where
  g = ruleGroup [r,r2]

  r = Rule { ruleName     = "Test rule"
           , ruleInputs   = Inputs { inputsWild = 1
                                   , inputsMaterial = bagFromList [ Raw B ]
                                   }
           , ruleProduces = [ bagFromList [ Move ] ]
           , ruleLongTerm = False
           }


  r2 = Rule { ruleName     = "Test Cont"
           , ruleInputs   = Inputs { inputsWild = 1
                                   , inputsMaterial = bagFromList [ Raw B ]
                                   }
           , ruleProduces = [ bagFromList [ Move ] ]
           , ruleLongTerm = True
           }



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
     sendJSON (historyCurrent (theGame h))

snapWithGame :: S -> (Factory -> Perhaps Factory) -> Snap ()
snapWithGame s upd =
  snapModifyMVar_ s $ \state ->
    case historyUpdateF upd (theGame state) of
      Ok h -> do let s1 = state { theGame = h }
                 sendJSON (historyCurrent h)
                 return s1
      Failed x -> badInput x

snapApply :: S -> Snap ()
snapApply s =
  do g <- snapParam "group"
     r <- snapParam "rule"
     m <- snapParam "material"
     snapWithGame s (factoryApply m g <=< factoryActivate r g)

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

