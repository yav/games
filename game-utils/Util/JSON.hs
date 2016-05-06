{-# LANGUAGE Trustworthy, OverloadedStrings #-}
module Util.JSON
  ( toBytes
  , null
  , bool
  , text
  , int
  , maybe
  , list
  , object
  , JS.Value
  ) where

import           Prelude hiding (null,maybe)
import           Data.Text(Text)
import qualified Data.Aeson as JS
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS


toBytes :: JS.Value -> LBS.ByteString
toBytes = JS.encode

null :: JS.Value
null = JS.Null

bool :: Bool -> JS.Value
bool = JS.Bool

text :: Text -> JS.Value
text t = JS.String t

int :: Integral a => a -> JS.Value
int = JS.Number . fromIntegral

list :: (a -> JS.Value) -> [a] -> JS.Value
list cvt xs = JS.Array (Vector.fromList (map cvt xs))

maybe :: (a -> JS.Value) -> Maybe a -> JS.Value
maybe cvt x = case x of
                  Nothing -> JS.Null
                  Just a  -> cvt a

object :: [ (Text,JS.Value) ] -> JS.Value
object = JS.object



