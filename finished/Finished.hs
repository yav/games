import Control.Monad(guard)
import Data.List(groupBy)
import Data.Maybe(fromMaybe)
import Util.Random(genRand, randSource, randSourceIO, shuffle, Gen)
import Text.Read(readMaybe)
import Data.List(intercalate,sortBy)
import Data.Function(on)

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
  do cs <- shuffle (filter ((48 /=) . number) cards)
     return $ drawCards 3 Game
        { coffee      = c
        , sweetSupply = 10 - s
        , sweets      = s
        , deck        = cs ++ filter ((48 ==) . number) cards
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
gainSweet g = if sweetSupply g > 0 then g { sweets = sweets g + 1
                                          , sweetSupply = sweetSupply g - 1 }
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
endTurn :: Bool -> Game -> Game
endTurn skipPast g =
  let (n,xs) = clean (present g)
      g1 = drinkCoffee
         $ bonusSweets g { sweetSupply = n + sweetSupply g, present = xs }
      g2 = if skipPast then g1 { deck = deck g1 ++ present g1 }
                       else g1 { past = past g1 ++ present g1 }
   in checkFuture (reducePast g2 { present = [] })


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
  do guard (sweets g >= 1)
     (as,b,bs) <- findInPlace ((n ==) . number) (present g)
     b1 <- activateCard b
     return (action b1, g { sweets = sweets g - 1, present = as ++ b1 : bs })

-- | Activate a card.
useCard :: Int -> (Game -> Game) -> (Game -> Game)
useCard n k g =
  case getCardAction n g of
    Just (a,g1) -> performAction a k g1
    Nothing     -> g

performAction :: Action -> (Game -> Game) -> (Game -> Game)
performAction a k =
  case a of
    StartScoring          -> id
    DrinkCoffee           -> id
    TakeSweets            -> id
    DrawCard              -> k . drawCard
    DrawCards2            -> k . drawCards 2
    BelowTheStack         -> belowTheStack k
    CardsIntoThePast      -> cardsIntoThePast k
    CardsFromThePast      -> k . cardsFromThePast
    OneCardIntoTheFuture  -> oneCardIntoTheFuture k
    AllCardsIntoTheFuture -> k . allCardsIntoTheFuture
    ExchangeCards         -> exchangeCards k
    RemoveSweets          -> k . removeSweets

getPresentCard :: Int -> Game -> Maybe (Card, Game)
getPresentCard n g =
  case findInPlace ((n ==) . number) (present g) of
    Just (as,b,bs) -> Just (b, g { present = as ++ bs })
    _              -> Nothing

cardsFromThePast :: Game -> Game
cardsFromThePast g =
  let n = length (past g)
      (as,bs) = splitAt (n - 2) (past g)
  in foldr toPresent g { past = as } bs

cardsIntoThePast :: (Game -> Game) -> (Game -> Game)
cardsIntoThePast k g0 = g1
  where
  cardNum = case present g0 of
              _ : _ : _ -> 2
              _ -> 1

  g1 = g0 { question = Just (ask g1 (cardNum == 2)) }

  ask g again =
    Question "Choose a card to send to the past." $ \i ->
    fromMaybe g $
      do (c,g2) <- getPresentCard i g
         let (n,c1) = clean c
             chCoffee = if action c == DrinkCoffee then 1 else 0
             g3 = g2 { sweetSupply = n + sweetSupply g2
                     , past = past g2 ++ [c1]
                     , coffee = coffee g2 - chCoffee
                     , question = Just (ask g3 False)
                     }
         return $ if again then g3
                           else k (drawCards cardNum g3 { question = Nothing })

play :: Game -> Game
play g = g1
  where
  g1 = g { question = Just (ask g1) }
  ask s = Question "Choose a card to activate, or 0" $ \i ->
            if i == 0
              then reorderCards (play . endTurn False) s
              else useCard i play s

reorderCards :: (Game -> Game) -> (Game -> Game)
reorderCards k g = g { question = Just auto }
  where
  g0 = g { present = sortBy (compare `on` number) (present g) }

  g1 = g0 { question = Just (ask g1) }

  auto = Question "Manual sort? 0: no, 1: yes." $ \i ->
         if i == 0
          then k g0
          else g1


  ask s = Question "Choose a card to move, or 0 to end." $ \i ->
            if i == 0
              then k s
              else
                case getPresentCard i s of
                  Nothing -> s
                  Just (a,g2) -> g2 { question = Just (askDest a g2) }

  askDest a s = Question "Choose a destination: " $ \i ->
                  case splitAt i (present s) of
                    (as,bs) -> let s1 = s { present = as ++ a : bs
                                          , question = Just (ask s1) }
                               in s1

belowTheStack :: (Game -> Game) -> (Game -> Game)
belowTheStack k = reorderCards (k . endTurn True)

oneCardIntoTheFuture :: (Game -> Game) -> (Game -> Game)
oneCardIntoTheFuture k g = g1
  where
  g1 = g { question = Just ask }
  ask = Question "Choose a card to send to the future." $ \i ->
        case getPresentCard i g of
          Just (c,g2) -> k g2 { futures = oneFutureCard c (futures g1)
                              , question = Nothing }
          Nothing     -> g1


allCardsIntoTheFuture :: Game -> Game
allCardsIntoTheFuture g =
  drawCards 3 g { present = []
                , futures = allFutureCards (present g) (futures g) }

exchangeCards :: (Game -> Game) -> (Game -> Game)
exchangeCards k g = g1 { question = Just ask }
  where
  g1  = (drawCard g) { question = Just ask }
  ask = Question "Choose a card to return." $ \i ->
        case getPresentCard i g1 of
          Just (c,g2) ->
            let (n,c1) = clean c
            in k g2 { deck = c1 : deck g1
                    , sweetSupply = sweetSupply g2 + n
                    , question = Nothing }
          Nothing     -> g1


removeSweets :: Game -> Game
removeSweets g =
  g { sweetSupply = n1 + n2 + sweetSupply g
    , present = cs
    , futures = ds
    }
  where
  ok c = number c /= 37
  (n1, cs) = cleanOnly ok (present g)
  (n2, ds) = cleanOnly ok (futures g)


-- | Answer a question.
continue :: Int -> Game -> Game
continue n g = case question g of
                 Just (Question _ k) -> k n
                 Nothing -> g


--------------------------------------------------------------------------------
-- View

showGame :: Game -> String
showGame g = unlines [ stats
                     , futureRows
                     , row "present" (present g)
                     , row "past" (past g), qu ]
  where
  stats = "sweets: " ++ show (sweets g) ++
          ", sweetSupply: " ++ show (sweetSupply g) ++
          ", coffee: " ++ show (coffee g) ++
          ", finished: " ++ show (length (finished g))

  rowChunk = intercalate "   " . map showCard
  row x    = unlines . (("--- " ++ x ++ " ------"):) . map rowChunk . chunks 4

  futureRows = concatMap futureRow (reverse (futures g))
  futureRow x = case x of
                 OneCard xs  -> row "future 1" xs
                 AllCards xs -> row "future all" xs

  qu = case question g of
         Nothing -> ""
         Just (Question a _) -> a

showCard :: Card -> String
showCard c = "[" ++ show (number c) ++ "|" ++ showAction (action c) ++
                    acts ++ "]"
  where
  acts = concat $ replicate (hasSweets c) full ++
                  replicate (supportsSweets c - hasSweets c) empty

  full = "(*)"
  empty = "( )"

showAction :: Action -> String
showAction a =
  case a of
    StartScoring          -> "start"
    DrinkCoffee           -> "coffee"
    TakeSweets            -> "sweet"
    DrawCard              -> "card"
    DrawCards2            -> "2 cards"
    BelowTheStack         -> "all to deck"
    CardsIntoThePast      -> "2 to past"
    CardsFromThePast      -> "2 from past"
    OneCardIntoTheFuture  -> "1 to future"
    AllCardsIntoTheFuture -> "all to future"
    ExchangeCards         -> "exchange"
    RemoveSweets          -> "clear"


--------------------------------------------------------------------------------
-- Helpers

chunks :: Int -> [a] -> [[a]]
chunks n xs = case xs of
                [] -> []
                _  -> case splitAt n xs of
                        (as,bs) -> as : chunks n bs

findInPlace :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
findInPlace p xs = case break p xs of
                     (as,b:bs) -> Just (as, b, bs)
                     _         -> Nothing

--------------------------------------------------------------------------------

main :: IO ()
main = go =<< newGameIO Regular
  where
  go s = case question s of
           Nothing -> if gameWon s then putStrLn "Victory!" else
                      if gameLost s then putStrLn "You loose." else
                      go (play s)
           Just _  -> do putStrLn (showGame s)
                         i <- getLine
                         case readMaybe i of
                           Just x -> go (continue x s)
                           Nothing -> go s












