{-# Language OverloadedStrings #-}
module State where

import Text.PrettyPrint
import Data.List(intersperse)
import Text.Read

data Order =
    Combined Combinator [Order]
  | Basic BasicOrder Resource

data Combinator =
    Separate    -- *
  | Together    -- \/
  | OneOf       -- +
  | AllOf       -- /\

data BasicOrder = Needs | Provides   -- ^ Polarity
              deriving Eq

type Resource = String

oppositeBasicOrder :: BasicOrder -> BasicOrder
oppositeBasicOrder b =
  case b of
    Needs -> Provides
    Provides -> Needs

oppositeCombinator :: Combinator -> Combinator
oppositeCombinator c =
  case c of
    Separate -> Together
    Together -> Separate
    OneOf    -> AllOf
    AllOf    -> OneOf

oppositeOrder :: Order -> Order
oppositeOrder g =
  case g of
    Combined c gs -> Combined (oppositeCombinator c) (map oppositeOrder gs)
    Basic b a     -> Basic (oppositeBasicOrder b) a

-- | A modifiable proof derivation.
data State = SSeparate [Order] [State] -- ^ unallocated goals, proof tree
           | SAllOf    [State]
           | STogether [Order]


splitOrder :: Order -> Maybe State
splitOrder goal =
  case goal of
    Combined s gs ->
      case s of
        Separate  -> Just (SSeparate [] (map one gs))
        AllOf     -> Just (SAllOf    (map one gs))
        Together  -> Just (STogether gs)
        OneOf     -> Nothing
    Basic {} -> Nothing
  where one g = STogether [g]

withPath :: (State -> State) -> [Int] -> State -> State
withPath act path state =
  case path of
    x : xs ->
      case state of
        SSeparate gs ss -> go ss $ \front newS back ->
          case newS of
            SSeparate gs1 ss1 -> SSeparate (gs ++ gs1) (front ++ ss1 ++ back)
            _                 -> SSeparate gs          (front ++ newS : back)
        SAllOf ss -> go ss $ \front newS back ->
          case newS of
            SAllOf ss1 -> SAllOf (front ++ ss1 ++ back)
            _          -> SAllOf (front ++ newS : back)

        STogether {}       -> state

      where go ss k = case splitAt x ss of
                       (front, this : back) ->
                          k front (withPath act xs this) back
                       _ -> state

    [] -> act state


withOrders :: ([Order] -> State) -> [Int] -> State -> State
withOrders act = withPath $ \state ->
  case state of
    STogether g -> act g
    _        -> state

withOrder ::
  ([Order] -> Order -> [Order] -> State) -> Int -> [Int] -> State -> State
withOrder act n = withOrders $ \goals ->
  case splitAt n goals of
    (before, this : after) -> act before this after
    _ -> STogether goals


stateAddOrders :: [Order] -> State -> State
stateAddOrders g state =
  case state of
    STogether gs    -> STogether (g ++ gs)
    SSeparate gs ss -> SSeparate (g ++ gs) ss
    SAllOf ss       -> SAllOf (map (stateAddOrders g) ss)

stateAddSeparateOrders :: [Order] -> State -> State
stateAddSeparateOrders gs state = SSeparate [] (state : map one gs)
  where one x = STogether [x]



stateSendOrders :: Int -> Int -> Int -> [Int] -> State -> State
stateSendOrders from num to = withPath $ \state ->
  case state of
    SSeparate gs ss
      | (gBefore,gRest)    <- splitAt from gs
      , (g,gAfter@(_:_))   <- splitAt num gRest
      , (sBefore,s:sAfter) <- splitAt to ss
      -> SSeparate (gBefore ++ gAfter) (sBefore ++ stateAddOrders g s : sAfter)
    _ -> state

stateSplitOrder :: Int -> [Int] -> State -> State
stateSplitOrder = withOrder $ \before this after ->
  case splitOrder this of
    Just s  -> stateAddOrders (before ++ after) s
    Nothing -> STogether (before ++ this : after)

stateChoose :: Int -> Int -> [Int] -> State -> State
stateChoose opt = withOrder $ \before this after ->
  case this of
    Combined OneOf gs | (_, g : _) <- splitAt opt gs ->
      STogether (before ++ g : after)
    _ -> STogether (before ++ this : after)

stateComplete :: [Int] -> State -> State
stateComplete = withPath $ \state ->
  case state of
    STogether [ Basic a1 r1, Basic a2 r2 ]
      | a1 /= a2 && r1 == r2 -> SSeparate [] []
    _ -> state

--------------------------------------------------------------------------------



prettyOrder :: Order -> Doc
prettyOrder goal =
  case goal of

    Combined c gs -> brackets sub
      where
      p = prettyCombinator c
      sub = case intersperse p (map prettyOrder gs) of
              [] -> p
              ys -> hsep ys

    Basic b r -> pref <> text r
      where pref = case b of
                     Needs    -> "-"
                     Provides -> "+"


prettyCombinator :: Combinator -> Doc
prettyCombinator c =
  case c of
    Separate -> ";"
    Together -> ","
    OneOf    -> "/"
    AllOf    -> "|"

prettyState :: State -> Doc
prettyState state =
  case state of
    STogether gs    -> vcat (map prettyOrder gs)
    SAllOf ss       -> "|" <+> vcat (map prettyState ss)
    SSeparate gs ss -> ";" <+> vcat [ vcat (map prettyOrder gs)
                                    , "---"
                                    , vcat (map prettyState ss)
                                    ]

prettyStateWithPath :: [Int] -> State -> Doc
prettyStateWithPath path s =
  case path of
    []     -> ">" <+> prettyState s
    x : xs ->
      case s of
        SAllOf ss
          | (front,this:back) <- splitAt x ss ->
            "|" <+> vcat (map prettyState front ++ prettyStateWithPath xs this :
                                                    map prettyState back)

        SSeparate gs ss
          | (front,this:back) <- splitAt x ss ->
            ";" <+> vcat [ vcat (map prettyOrder gs)
                         , "---"
                         , vcat (map prettyState front ++
                                     prettyStateWithPath xs this :
                                     map prettyState back) ]

        _ -> prettyState s $$ "> ???"



--------------------------------------------------------------------------------

x .* y  = Combined Separate [x,y]
x .+ y  = Combined OneOf    [x,y]
x \/ y  = Combined Together [x,y]
x /\ y  = Combined AllOf    [x,y]

var x   = Basic Provides x
var' x  = oppositeOrder (var x)

ex1 = (a .* (b .+ c)) \/ oppositeOrder ((a .* b) .+ (a .* c))
  where
  a = var "A"
  b = var "B"
  c = var "C"

play :: Order -> IO ()
play g = go [] (STogether [g])
  where
  go path s =
    do print (prettyStateWithPath path s)
       l <- getLine
       case words l of
         ["f"] -> go path (stateComplete path s)
         ["e"] -> go path (stateSplitOrder 0 path s)
         ["e",x] | Just n <- readMaybe x -> go path (stateSplitOrder n path s)

         [">",x] | Just n <- readMaybe x -> go (path ++ [n]) s
         ["^"] | _ : _ <- path -> go (init path) s

         _ -> putStrLn "Don't understand" >> go path s




