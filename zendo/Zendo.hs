module Zendo
  ( Color (..)
  , Shape (..)
  , Item  (..)
  , Example
  , Game (..)
  , Term(..)
  , Prop(..)
  , newGame
  , addExample
  , guessRule
  , maxItems
  , maxRow
  , maxCol
  ) where

import SimpleSMT hiding (Value,and,or,not)
import qualified SimpleSMT as SMT
import Prelude hiding (const)

data Color    = Red | Green | Blue deriving (Read,Show,Eq)
data Shape    = Circle | Triangle | Square deriving (Read,Show,Eq)
data Item     = Item { color :: Color, shape :: Shape, at :: (Int,Int) }
                deriving (Show,Eq)

type Example = [Item]

data Game     = Game { rule         :: Prop
                     , goodExamples :: [Example]
                     , badExamples  :: [Example]
                     , solver       :: Solver
                     }

type Name = String

data Term =
    Var Name
  | ColorOf Term
  | ShapeOf Term
  | LocationOf Term
  | RowOf Term
  | ColOf Term
  | Count Name Prop
  | Num Int
  | Color Color
  | Shape Shape
    deriving Show

data Prop =
    Term :=: Term
  | Term :>=: Term
  | Touching Term Term
  | And [Prop]
  | Not Prop
  | Exists Name Prop
    deriving Show

data Value = VColor Color | VShape Shape | VLocation (Int,Int)
           | VItem Item | VNum Int
            deriving Eq


evalProp :: [(Name,Item)] -> [Item] -> Prop -> Bool
evalProp env is prop =
  case prop of
    t1 :=: t2 -> evalTerm env is t1 == evalTerm env is t2

    Num x :>=: Num y -> x >= y
    _     :>=: _ -> error "Malformed prop"

    And ps    -> and (map (evalProp env is) ps)

    Not p -> not (evalProp env is p)

    Exists x p -> or [ evalProp ((x,i):env) is p | i <- is ]

    Touching t1 t2
      | VItem Item { at = (x,y) } <- evalTerm env is t1
      , VItem Item { at = (x',y') } <- evalTerm env is t2 ->
        or [ x == x' + 1, x' == x + 1, y == y' + 1, y' == y + 1 ]
      | otherwise -> error "malformed property"

evalTerm :: [(Name,Item)] -> [Item] -> Term -> Value
evalTerm env is term =
  case term of
    Var x        | Just v          <- lookup x env      -> VItem v
                 | otherwise -> error "Malformed term"
    ColorOf t    | VItem v         <- evalTerm env is t -> VColor (color v)
                 | otherwise -> error "Malformed term"
    ShapeOf t    | VItem v         <- evalTerm env is t -> VShape (shape v)
                 | otherwise -> error "Malformed term"
    LocationOf t | VItem v         <- evalTerm env is t -> VLocation (at v)
                 | otherwise -> error "Malformed term"
    RowOf t      | VLocation (_,y) <- evalTerm env is t -> VNum y
                 | otherwise -> error "Malformed term"
    ColOf t      | VLocation (x,_) <- evalTerm env is t -> VNum x
                 | otherwise -> error "Malformed term"
    Count x p -> VNum $ sum [ 1 | i <- is, evalProp ((x,i):env) is p ]
    Num t     -> VNum t
    Color t   -> VColor t
    Shape t   -> VShape t


--------------------------------------------------------------------------------

itemName :: Int -> String
itemName x = "item" ++ show x

smtTerm :: [SExpr] -> [(Name,SExpr)] -> Term -> SExpr
smtTerm itemNames = go
  where
  go env term =
    case term of

      Var x ->
        case lookup x env of
          Just s -> s
          Nothing -> error ("Undefined variable: " ++ show x)

      ColorOf x -> fun "color" [ go env x ]
      ShapeOf x -> fun "shape" [ go env x ]
      LocationOf x -> fun "at" [ go env x ]

      RowOf x -> fun "row" [ go env x ]
      ColOf x -> fun "col" [ go env x ]

      Num x   -> int (fromIntegral x)

      Color x -> const (show x)
      Shape x -> const (show x)

      Count x p -> addMany (map count itemNames)
        where count i = ite (smtProp itemNames ((x,i):env) p) (int 1) (int 0)

smtProp :: [SExpr] -> [(Name,SExpr)] -> Prop -> SExpr
smtProp is env prop =
  case prop of
    x :=: y  -> eq (smtTerm is env x) (smtTerm is env y)
    x :>=: y -> geq (smtTerm is env x) (smtTerm is env y)
    Not p    -> SMT.not (smtProp is env p)
    Touching x y -> fun "item-touching" [ smtTerm is env x, smtTerm is env y ]
    And ps -> andMany (map (smtProp is env) ps)
    Exists x p -> orMany (map checkBody is)
      where
      checkBody i = smtProp is ((x,i):env) p

--------------------------------------------------------------------------------

tItem :: SExpr
tItem = const "Item"

declareItem :: Solver -> Int -> IO SExpr
declareItem s i =
  do item <- declare s (itemName i) tItem
     assert s (fun "item-valid" [item])
     return item

declareItems :: Solver -> Int -> IO [SExpr]
declareItems s num =
  do items <- mapM (declareItem s) (take num [ 0 .. ])
     assert s (distinct items)
     return items

parseItem :: SMT.Value -> Item
parseItem (SMT.Other (SMT.List [ SMT.Atom "item"
                               , SMT.Atom col
                               , SMT.Atom sh
                               , SMT.List [ SMT.Atom "loc"
                                          , SMT.Atom x
                                          , SMT.Atom y
                                          ] ])) =
  Item { color = read col, shape = read sh, at = (read x, read y) }
parseItem _ = error "Invalid Item"

maxItems :: Int
maxItems = 3

maxRow :: Int
maxRow = 5

maxCol :: Int
maxCol = 5

newGame :: Prop -> IO Game
newGame p =
  do s <- newSolver "z3" ["-smt2", "-in"] Nothing
     loadFile s "Zendo.z3"
     mb1 <- findExample s (\is -> smtProp is [] p)
     mb2 <- findExample s (\is -> SMT.not $ smtProp is [] p)
     case (mb1,mb2) of
       (Just good, Just bad) -> return Game { rule = p
                                            , goodExamples = [good]
                                            , badExamples = [bad]
                                            , solver = s
                                            }
       _ -> stop s >> fail "Invalid rule"


findExample :: Solver -> ([SMT.SExpr] -> SMT.SExpr) -> IO (Maybe Example)
findExample s p = go maxItems
  where
  go :: Int -> IO (Maybe Example)
  go n =
    do mb <- inNewScope s $
               do is <- declareItems s n
                  assert s (p is)
                  ok <- check s
                  let names = [ itemName i | (i,_) <- zip [ 0 .. ] is ]
                  case ok of
                    Sat -> Just . map (parseItem . snd) <$> getConsts s names
                    _   -> return Nothing
       case mb of
         Just xs -> return (Just xs)
         Nothing | n > 1 -> go (n - 1)
                 | otherwise -> return Nothing


addExample :: Example -> Game -> Game
addExample is g
  | is `elem` goodExamples g || is `elem` badExamples g = g
  | evalProp [] is (rule g) = g { goodExamples = is : goodExamples g }
  | otherwise = g { badExamples = is : badExamples g }

guessRule :: Game -> Prop -> IO (Maybe Game)
guessRule g p
  | any isBad  (goodExamples g) = return (Just g)
  | any isGood (badExamples  g) = return (Just g)
  | otherwise =
    do mb <- findExample (solver g) (\is -> SMT.not $ smtProp is [] p `eq`
                                                      smtProp is [] (rule g))
       case mb of
         Nothing -> SMT.stop (solver g) >> return Nothing
         Just is -> return (Just (addExample is g))
  where
  isGood is = evalProp [] is p
  isBad is  = not (evalProp [] is p)


