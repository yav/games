import Control.Monad(guard)
import Data.List(groupBy)

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

-- | Remove sweets from a card.
cleanCard :: Card -> (Int, Card)
cleanCard c = (hasSweets c, c { hasSweets = 0 })





data Game = Game
  { coffee      :: Int
  , sweetSupply :: Int
  , sweets      :: Int
  , deck        :: [Card]
  , finished    :: [Card]
  , present     :: [Card]
  , past        :: [Card]     -- ^ Oldest first
  , futures     :: [[Card]]   -- ^ Closest future first
  }

gameWon :: Game -> Bool
gameWon g =
  case finished g of
    c : _ -> number c == 48
    _     -> False

gameLost :: Game -> Bool
gameLost g = coffee g == 0

gameFinished :: Game -> Bool
gameFinished g = gameWon g || gameLost g

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

-- | Add this card to the present
toPresent :: Card -> Game -> Game
toPresent c g =
  case finished g of
    [] | number c == 1 -> drawCard g { finished = [c] }
    x : xs | number c == succ (number x) ->
                          drawCard g { finished = c : x : xs }
    _ -> g { present = c : present g }

-- | Draw 3 cards at the start of turn.
startOfTurn :: Game -> Game
startOfTurn = drawCard . drawCard . drawCard

-- | Check if there are future cards to use.  Otherwise, start a new turn.
checkFuture :: Game -> Game
checkFuture g =
  case futures g of
    [] -> startOfTurn g
    f : fs -> foldr toPresent g { futures = fs } f

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
    Just (a,g1) ->
      case a of
        -- shouldn't happen
        StartScoring -> g
        DrinkCoffee  -> g
        TakeSweets   -> g

        DrawCard   -> drawCard g1
        DrawCards2 -> drawCard (drawCard g1)
        BelowTheStack -> undefined
        CardsIntoThePast -> undefined
        CardsFromThePast -> undefined
        OneCardIntoTheFuture -> undefined
        AllCardsIntoTheFuture -> undefined
        ExchangeCards -> undefined
        RemoveSweets -> undefined

    Nothing -> g






