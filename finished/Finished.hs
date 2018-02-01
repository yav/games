module Finished
  ( Difficulty(..)
  , newGame
  , newGameIO
  , Game
  , gameWon
  , gameLost
  , useCard
  , continue
  , endTurn
  ) where

import Control.Monad(guard)
import Data.List(groupBy)
import Data.Maybe(fromMaybe)
import Util.Random(genRand, randSource, randSourceIO, shuffle, Gen)

data Action = StartScoring
            | TakeSweets
            | DrinkCoffee

            -- require a sweet to activate
            | DrawCards2
            | DrawCard
            | BelowTheStack
            | CardsIntoThePast
            | CardsFromThePast
            | OneCardIntoTheFuture
            | AllCardsIntoTheFuture
            | ExchangeCards
            | RemoveSweets
              deriving Eq

data Card = Card { number :: Int
                 , action :: Action
                 , supportsSweets :: Int
                 , hasSweets :: Int
                 }

infix 6 |->

card :: Int -> Action -> Int -> Card
card s a n = Card { number = n, action = a, supportsSweets = s, hasSweets = 0 }

(|->) :: [Int] -> (Int -> Card) -> [Card]
xs |-> f = map f xs

cards :: [Card]
cards = concat
  [ [1] |-> card 0 StartScoring
  , [3,6,10,15,21,28,36,45] |-> card 0 TakeSweets
  , [48] |-> card 0 DrinkCoffee
  , [2] |-> card 1 DrawCards2
  , [9,14,20,27,31,34,46] |-> card 1 DrawCard
  , [47] |-> card 3 DrawCard
  , [4,7,29,42] |-> card 1 BelowTheStack
  , [5,11,17,23,25,41] |-> card 1 CardsIntoThePast
  , [8,18,30,44] |-> card 1 CardsFromThePast
  , [12,19,32,40] |-> card 1 OneCardIntoTheFuture
  , [16,24,26,35,38] |-> card 1 AllCardsIntoTheFuture
  , [13,22,33,39,43] |-> card 1 ExchangeCards
  , [37] |-> card 1 RemoveSweets
  ]

-- | Add a sweet to a card, if possible
activateCard :: Card -> Maybe Card
activateCard c
  | hasSweets c < supportsSweets c = Just c { hasSweets = hasSweets c + 1 }
  | otherwise = Nothing

data Difficulty = VeryEasy | Easy | Regular | Difficult

newGame :: Int -> Difficulty -> Game
newGame s d = fst $ genRand (randSource s) $ setupGame d

newGameIO :: Difficulty -> IO Game
newGameIO d = do s <- randSourceIO
                 return $ fst $ genRand s $ setupGame d



-- | Setup a game, without shuffling the cards
setupGame :: Difficulty -> Gen Game
setupGame d =
  do cs <- shuffle cards
     return $ drawCards 3 Game
        { coffee      = c
        , sweetSupply = 10 - s
        , sweets      = s
        , deck        = cs
        , finished    = []
        , present     = []
        , past        = []
        , futures     = []
        , question    = Nothing
        }
  where
  (c,s) = case d of
            VeryEasy  -> (7,7)
            Easy      -> (7,5)
            Regular   -> (6,5)
            Difficult -> (5,5)

data Question = Question String (Int -> Game)

data Game = Game
  { coffee       :: Int
  , sweetSupply  :: Int
  , sweets       :: Int
  , deck         :: [Card]
  , finished     :: [Card]
  , present      :: [Card]
  , past         :: [Card]     -- ^ Oldest first
  , futures      :: [Future]
  , question     :: Maybe Question
  }

data Future = OneCard [Card] | AllCards [Card]

-- | Add a card to the future, using the "1 card to the future" action.
oneFutureCard :: Card -> [Future] -> [Future]
oneFutureCard c fs =
  case fs of
    []                -> [ OneCard [ c ] ]
    OneCard xs : xss  -> OneCard (c : xs) : xss
    AllCards xs : xss -> AllCards (c : xs) : xss

-- | Add a card to the future, using the "all cards to the future" action.
allFutureCards :: [Card] -> [Future] -> [Future]
allFutureCards cs fs =
  case fs of
    []                -> [ AllCards cs ]
    OneCard xs  : xss -> AllCards (cs ++ xs) : xss
    AllCards xs : xss -> AllCards cs : AllCards xs : xss

-- | Remove the sweets from things.
class Clean a where
  cleanOnly :: (Card -> Bool) -> a -> (Int, a)

instance Clean a => Clean [a] where
  cleanOnly p xs = let (ns,ys) = unzip (map (cleanOnly p) xs) in (sum ns, ys)

instance Clean Card where
  cleanOnly p c = if p c then (hasSweets c, c { hasSweets = 0 })
                         else (0, c)

instance Clean Future where
  cleanOnly p f =
    case f of
      OneCard cs  -> let (n,ds) = cleanOnly p cs in (n, OneCard ds)
      AllCards cs -> let (n,ds) = cleanOnly p cs in (n, AllCards ds)

clean :: Clean a => a -> (Int, a)
clean = cleanOnly (const True)

-- | Has the game been won?
gameWon :: Game -> Bool
gameWon g =
  case finished g of
    c : _ -> number c == 48
    _     -> False

-- | Has the game been lost?
gameLost :: Game -> Bool
gameLost g = coffee g <= 0


-- | Gain a sweet, if one is available.
gainSweet :: Game -> Game
gainSweet g = if sweetSupply g > 0 then g { sweetSupply = sweetSupply g - 1 }
                                   else g

-- | Draw a card from the deck, or the past, if the deck is empty.
drawCard :: Game -> Game
drawCard g =
  case deck g of
    c : cs -> toPresent c $ case action c of
                              TakeSweets -> gainSweet g { deck = cs }
                              _          -> g { deck = cs }
    []  -> case past g of
             c : cs -> toPresent c g { past = cs }
             []     -> g

drawCards :: Int -> Game -> Game
drawCards n g = iterate drawCard g !! n

-- | Add this card to the present
toPresent :: Card -> Game -> Game
toPresent c g =
  case finished g of
    [] | number c == 1 -> drawCard g { finished = [c] }
    x : xs | number c == succ (number x) ->
                          drawCard g { finished = c : x : xs }
    _ -> g { present = c : present g }

-- | Check if there are future cards to use.  Otherwise, start a new turn.
checkFuture :: Game -> Game
checkFuture g =
  case futures g of
    [] -> drawCards 3 g
    OneCard xs : xss -> drawCards 3 (newPresent xs g { futures = xss })
    AllCards xs : xss -> newPresent xs g { futures = xss }
  where
  newPresent xs g1 = foldr toPresent g1 xs

-- | More present to past, gain bonuses, reduce past, then redraw.
endTurn :: Game -> Game
endTurn g =
  let (n,xs) = clean (present g)
      g1 = drinkCoffee
         $ bonusSweets g { sweetSupply = n + sweetSupply g, present = xs }
   in checkFuture (reducePast g1 { present = [], past = past g1 ++ present g1 })


-- | Check the present for bonus sweets.
bonusSweets :: Game -> Game
bonusSweets g = iterate gainSweet g !!
               ( sum
               $ map (subtract 1)
               $ filter (>= 3)
               $ map length
               $ groupBy sequential
               $ present g
               )
  where
  sequential x y = succ (number x) == number y

-- | Drink coffee for any relevant cards in the present.
drinkCoffee :: Game -> Game
drinkCoffee g = g { coffee = coffee g - n }
  where n = length (filter ((DrinkCoffee ==) . action) (present g))

-- | Move all but 3 cards from the past to the bottom of the deck.
reducePast :: Game -> Game
reducePast g = if n <= 3 then g else g { past = bs, deck = deck g ++ as }
  where n       = length (past g)
        (as,bs) = splitAt (n - 3) (past g)


-- | Get the action for a card, and pay for it.
getCardAction :: Int -> Game -> Maybe (Action,Game)
getCardAction n g =
  do guard (sweets g > 1)
     (as,b:bs) <- return (splitAt n (present g))
     b1 <- activateCard b
     return (action b1, g { present = as ++ b1 : bs })

-- | Activate a card.
useCard :: Int -> Game -> Game
useCard n g =
  case getCardAction n g of
    Just (a,g1) -> performAction a g1
    Nothing     -> g

performAction :: Action -> Game -> Game
performAction a =
  case a of
    StartScoring          -> id
    DrinkCoffee           -> id
    TakeSweets            -> id
    DrawCard              -> drawCard
    DrawCards2            -> drawCards 2
    BelowTheStack         -> belowTheStack
    CardsIntoThePast      -> cardsIntoThePast
    CardsFromThePast      -> cardsFromThePast
    OneCardIntoTheFuture  -> oneCardIntoTheFuture
    AllCardsIntoTheFuture -> allCardsIntoTheFuture
    ExchangeCards         -> exchangeCards
    RemoveSweets          -> removeSweets

getPresentCard :: Int -> Game -> Maybe (Card, Game)
getPresentCard n g =
  case splitAt n (present g) of
    (as,b:bs) -> Just (b, g { present = as ++ bs })
    _         -> Nothing

cardsFromThePast :: Game -> Game
cardsFromThePast g =
  let n = length (past g)
      (as,bs) = splitAt (n - 2) (past g)
  in foldr toPresent g { past = as } bs

cardsIntoThePast :: Game -> Game
cardsIntoThePast g0 =
  case present g0 of
    _ : _ : _ -> g0 { question = Just (ask g0 True) }
    _ -> g0
  where
  ask g again =
    Question "Choose card from the past." $ \i ->
    fromMaybe g $
      do (c,g1) <- getPresentCard i g
         let (n,c1) = clean c
             chCoffee = if action c == DrinkCoffee then 1 else 0
             g2 = g1 { sweetSupply = n + sweetSupply g1
                     , past = past g1 ++ [c1]
                     , coffee = coffee g1 - chCoffee
                     }
         return $ if again then g2 { question = Just (ask g2 False) }
                           else drawCards 2 g2 { question = Nothing }

belowTheStack :: Game -> Game
belowTheStack g = reducePast g2 { deck = deck g1 ++ xs }
  where
  (s,xs) = clean (present g)
  g1     = g { sweetSupply = s + sweetSupply g, present = xs }
  g2     = drinkCoffee (bonusSweets g1)

oneCardIntoTheFuture :: Game -> Game
oneCardIntoTheFuture g = g { question = Just ask }
  where
  ask = Question "Chooase a card from the present." $ \i ->
        fromMaybe g
        $ do (c,g1) <- getPresentCard i g
             return g1 { futures = oneFutureCard c (futures g1)
                       , question = Nothing }


allCardsIntoTheFuture :: Game -> Game
allCardsIntoTheFuture g =
  drawCards 3 g { present = []
                , futures = allFutureCards (present g) (futures g) }

exchangeCards :: Game -> Game
exchangeCards g = (drawCard g) { question = Just ask }
  where
  ask = Question "Choose a card from the present." $ \i ->
          fromMaybe g
        $ do (c,g1) <- getPresentCard i g
             return g1 { deck = c : deck g, question = Nothing }


removeSweets :: Game -> Game
removeSweets g =
  g { sweetSupply = n1 + n2 + sweetSupply g
    , present = cs
    , futures = ds
    }
  where
  ok c = supportsSweets c <= 1
  (n1, cs) = cleanOnly ok (present g)
  (n2, ds) = cleanOnly ok (futures g)


-- | Answer a question.
continue :: Int -> Game -> Game
continue n g = case question g of
                 Just (Question _ k) -> k n
                 Nothing -> g



