-- | A non-empty list.
module Util.List1 where

newtype List1 a = List1 [a]

singleton :: a -> List1 a
singleton a = List1 [a]

toList :: List1 a -> [a]
toList (List1 xs) = xs

top :: List1 a -> a
top (List1 (x:_)) = x

pop :: List1 a -> Maybe (List1 a)
pop (List1 (_:xs)) = case xs of
                        [] -> Nothing
                        _  -> Just (List1 xs)

push :: a -> List1 a -> List1 a
push x (List1 xs) = List1 (x : xs)



