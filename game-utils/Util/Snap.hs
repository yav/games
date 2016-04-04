{-# LANGUAGE OverloadedStrings #-}
module Util.Snap where

import Util.JSON(Export,jsonBytes)

import           Snap.Core (Snap, bracketSnap)
import qualified Snap.Core as Snap

import           Data.ByteString(ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding(decodeUtf8,encodeUtf8)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text
import           Data.Char(toLower)

import           Control.Monad.IO.Class(liftIO)
import           Control.Concurrent(MVar, takeMVar, putMVar)
import           Data.IORef(newIORef, readIORef, writeIORef)

snapIO :: IO a -> Snap a
snapIO = liftIO

snapModifyMVar :: MVar a -> (a -> Snap (a, b)) -> Snap b
snapModifyMVar m f =
  bracketSnap start end $ \r ->
    do a       <- snapIO (readIORef r)
       (a1, b) <- f a
       snapIO (writeIORef r a1)
       return b

  where
  start = newIORef =<< takeMVar m
  end r = putMVar m =<< readIORef r


snapModifyMVar_ :: MVar a -> (a -> Snap a) -> Snap ()
snapModifyMVar_ m f = snapModifyMVar m f1
  where f1 a = do a1 <- f a
                  return (a1, ())





sendJSON :: Export a => a -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (jsonBytes a)

--------------------------------------------------------------------------------
-- Error reporting

badInput :: Text -> Snap a
badInput msg =
  Snap.finishWith (Snap.setResponseStatus 400
                                (encodeUtf8 msg) Snap.emptyResponse)

notFound :: Snap a
notFound = Snap.finishWith (Snap.setResponseStatus 404 "Not Found"
                                                      Snap.emptyResponse)


--------------------------------------------------------------------------------
-- Parameters

class SnapParam t where
  snapParam :: Text -> Snap t

instance SnapParam ByteString where
  snapParam p =
    do mb <- Snap.getParam (encodeUtf8 p)
       case mb of
         Just x  -> return x
         Nothing -> badInput ("Missing parameter: " `Text.append` p)

instance SnapParam Text where
  snapParam p = decodeUtf8 `fmap` snapParam p

instance SnapParam Int where
  snapParam p =
    do txt <- snapParam p
       let (neg,numTxt) = case Text.uncons txt of
                            Just ('-',t) -> (negate, t)
                            _            -> (id, txt)
       case decimal numTxt of
         Right (a,t) | Text.null t -> return (neg a)
         _ -> badInput ("Malformed integer parameter: " `Text.append` p)

instance SnapParam Word where
  snapParam p =
    do txt <- snapParam p
       case decimal txt of
         Right (a,t) | Text.null t -> return a
         _ -> badInput ("Malformed natural parameter: " `Text.append` p)


instance SnapParam Bool where
  snapParam = snapParamSimpleEnum

snapParamCases :: [(Text,a)] -> Text -> Snap a
snapParamCases cases p =
  do txt <- snapParam p
     case lookup txt cases of
       Just a   -> return a
       Nothing  -> badInput ("Malformed parameter: " `Text.append` p)

-- | WARNING: Assumes that values have different representations when converted
-- to lower case
snapParamSimpleEnum :: (Show a, Enum a, Bounded a) => Text -> Snap a
snapParamSimpleEnum = snapParamCases (map mk [ minBound .. maxBound ])
  where
  mk x = (Text.pack (map toLower (show x)), x)




