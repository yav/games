data Action = StartScoring
            | TakeSweets
            | DrinkCoffee

            -- require a sweet to activate
            | DrawCards2
            | DrawCard
            | DrawCard_x3
            | BelowTheStack
            | CardsIntoThePast
            | CardsFromThePast
            | OneCardIntoTheFuture
            | AllCardsIntoTheFuture
            | ExchangeCards
            | RemoveSweets

data Card = Card { number :: Int, action :: Action }

infix 6 |->
xs |-> y = [ Card { number = x, action = y } | x <- xs ]

cards = concat
  [ [1] |-> StartScoring
  , [3,6,10,15,21,28,36,45] |-> TakeSweets
  , [48] |-> DrinkCoffee
  , [2] |-> DrawCards2
  , [9,14,20,27,31,34,46] |-> DrawCard
  , [47] |-> DrawCard_x3
  , [4,7,29,42] |-> BelowTheStack
  , [5,11,17,23,25,41] |-> CardsIntoThePast
  , [8,18,30,44] |-> CardsFromThePast
  , [12,19,32,40] |-> OneCardIntoTheFuture
  , [16,24,26,35,38] |-> AllCardsIntoTheFuture
  , [13,22,33,39,43] |-> ExchangeCards
  , [37] |-> RemoveSweets
  ]

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

fillPresent g =
  case futures g of
    fs : more -> g { present = fs, futures = more }
    [] ->
      case splitAt 3 (deck g) of
        (as,rest@(_:_)) -> g { present = as ++ present g, deck = rest }
        (as, []) ->
          let (bs,cs) = splitAt (3 - length as) (past g)
          in g { present = as ++ bs, deck = [], past = cs }

takeCandy g =
  let n = min (sweetSupply g)
               (sum [ 1 | TakeSweets <- map action (present g) ])






