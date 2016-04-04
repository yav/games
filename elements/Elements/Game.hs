{-# LANGUAGE OverloadedStrings #-}
module Elements.Game where

import Control.Monad(when,ap,liftM,replicateM,join,unless)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text(Text)
import Data.Either(partitionEithers)

import Elements.BasicTypes
import Elements.Terrain
import Elements.GameTile
import Elements.Land
import Util.Bag
import Util.Random
import Util.Perhaps



data Resource = Movement | Block | Attack | Currency | Heal

              | CharacterFlaw

              -- Spirits
              | Summon | Spirit Spirit SpiritState
              | Friendly Element | Summoned Element

              -- Actions
              | Action Action
              | UsedAction Action
                deriving (Eq,Ord,Show)

data SpiritState = IsAvailable
                 | WasSummoned
                 | WasTransformed
                   deriving (Eq,Ord,Show)

data Action   = Act { actName     :: Text
                    , actElement  :: Element
                    , actBase     :: ActPart
                    , actAdvanced :: ActPart
                    }

data ActPart  = ActPart { actDo :: IM (), actHelp :: Description }


instance Show Action where
  show a = show (actName a)

instance Eq Action where
  x == y = actName x == actName y

instance Ord Action where
  compare x y = compare (actName x) (actName y)



data Game = Game
  { gameResources :: Resources    -- ^ Our resources
  , gamePast      :: [Action]     -- ^ Used up actions
  , gameFuture    :: [Action]     -- ^ Potential actions
  , gameAlignment :: Int          -- ^ Current alignment
  , gameActLimit  :: Int          -- ^ Default max actions per turn
  , gameLocation  :: Addr         -- ^ Our location

  , gameTerrainCosts  :: Map Terrain Int
    -- ^ Current terain costs

  , gameLand      :: Land
  , gameRNG       :: StdGen
  }



type Resources  = Bag Resource





newGame :: StdGen -> LandSetup -> Perhaps Game
newGame rng lsetup = genRandFun rng $
  do as   <- shuffle enumerate
     mbL  <- setupLand lsetup
     return $ \newRng ->
       do land <- mbL
          return $ endTurn $
            Game { gameResources  = bagAdd 5 (Spirit Birth WasSummoned) bagEmpty
                 , gamePast       = []
                 , gameFuture     = as
                 , gameActLimit   = 5
                 , gameAlignment  = 0
                 , gameLocation   = startLoc
                 , gameTerrainCosts = Map.empty

                 , gameLand       = land
                 , gameRNG        = newRng
                 }

startLoc :: Addr
startLoc = Addr { addrGlobal = (0,0), addrLocal = Center }

startTurn :: Game -> Game
startTurn g = g { gameResources = foldr (bagAdd 1 . Action)
                                        (bagAdd 5 Summon (gameResources g))
                                        new
                , gameFuture = rest
                }
  where
  (new,rest) = splitAt (gameActLimit g - acts) (gameFuture g)
  acts       = sum [ n | (Action _, n) <- bagToListGrouped (gameResources g) ]

endTurn :: Game -> Game
endTurn g0 = check g0 { gameResources = bagAdd 100 Movement bagEmpty
                      , gameTerrainCosts = terrainCosts
                      }
                   (bagToListGrouped (gameResources g0))
  where
  add n x g = g { gameResources = bagAdd n x (gameResources g) }

  check newG stats =
    case stats of
      []            -> newG
      (ty,amt) : ss ->
        case ty of
          Movement      -> check newG ss
          Block         -> check newG ss
          Attack        -> check newG ss
          Currency      -> check newG ss
          Heal          -> check newG ss
          Summoned _    -> check newG ss
          Summon        -> check newG ss

          CharacterFlaw ->
            let newA = max (-7) (gameAlignment newG - 1)
            in check newG { gameAlignment = newA } ss

          Spirit p s ->
            case s of
              IsAvailable    -> check (addS amt p newG) ss
              WasTransformed -> check (addS amt p newG) ss
              WasSummoned | p == Death -> check (addS amt p newG) ss
              WasSummoned    -> check (foldr (addS 1) newG' ps) ss
                where
                newG'    = newG { gameRNG = rng }
                (ps,rng) = genRand (gameRNG newG)
                         $ replicateM amt $ oneOf enumerate
            where
            addS n t = add n (Spirit t IsAvailable)

          Friendly x    -> check (add (min 3 amt) (Friendly x) newG) ss
          Action a      -> check (add amt (Action a) newG) ss
          UsedAction a  -> check newG { gamePast = a : gamePast newG } ss


--------------------------------------------------------------------------------

data Interact   = ChooseNum [Description] (Int -> Interact)
                | Spend Int Resource Interact
                | Gain  Int Resource Interact
                | ExtraAction Interact
                | GetGame (Game -> Interact)
                | UpdGame (Game -> Game) Interact
                | Fail
                | Done

newtype IM a    = IM ((a -> Interact) -> Interact)


chooseElement  :: IM Element
chooseElement   = chooseNum [ (imgElement e, e) | e <- enumerate ]

chooseSpirit   :: IM Spirit
chooseSpirit    = chooseNum [ (imgSpirit e, e) | e <- enumerate ]

chooseAction   :: IM Action
chooseAction    = do chooseNum [ (ActionD a, a) | a <- enumerate ]

chooseNum      :: [(Description,a)] -> IM a
chooseNum xs    = IM $ \k -> ChooseNum (map fst xs) $ \n -> k (snd (xs !! n))

spend          :: Int -> Resource -> IM ()
spend n s       = IM (\k -> Spend n s (k ()))

gain           :: Int -> Resource -> IM ()
gain n s        = IM (\k -> Gain n s (k ()))

backtrack      :: IM a
backtrack       = IM (\_ -> Fail)

extraAction    :: IM ()
extraAction     = IM (\k -> ExtraAction (k ()))

choices        :: [ (Description,IM a) ] -> IM a
choices xs      = join (chooseNum xs)

getGame        :: IM Game
getGame         = IM GetGame

updGame        :: (Game -> Game) -> IM ()
updGame f       = IM (\k -> UpdGame f (k ()))

doInteract     :: Interact -> IM ()
doInteract i0   = IM (\k -> onDone (k ()) i0)
  where
  onDone fin i =
    case i of
      Done            -> fin
      Fail            -> Fail
      ChooseNum n k   -> ChooseNum n   (onDone fin . k)

      Spend n s k     -> Spend n s     (onDone k fin)
      Gain  n s k     -> Gain  n s     (onDone k fin)
      ExtraAction k   -> ExtraAction   (onDone k fin)

      GetGame k       -> GetGame       (onDone fin . k)
      UpdGame f k     -> UpdGame f     (onDone k fin)



instance Functor IM where
  fmap = liftM

instance Applicative IM where
  pure a = IM (\k -> k a)
  (<*>)  = ap

instance Monad IM where
  IM m >>= f    = IM $ \k -> m $ \a -> let IM m1 = f a
                                       in m1 k


--------------------------------------------------------------------------------


data ChoiceMap = NumChoices Game (Map Int (Description,ChoiceMap))
               | NoChoice   Game

runInteract :: Interact -> Game -> Either String ChoiceMap
runInteract i g =
  case i of
    Done            -> return (NoChoice g)
    Fail            -> Left "Fail"

    GetGame k       -> runInteract (k g) g
    UpdGame f k     -> runInteract k (f g)

    ExtraAction k   ->
      case gameFuture g of
        []     -> Left "No more actions" -- Or, treat as no-op?
        a : as -> runInteract k
                    g { gameFuture = as
                      , gameResources  = bagAdd 1 (Action a) (gameResources g)
                      }

    Gain n s k ->
      runInteract k g { gameResources = bagAdd n s (gameResources g) }

    Spend n s k     ->
      do s1 <- case bagRemove n s (gameResources g) of
                 Just x -> return x
                 Nothing ->
                   Left $ unwords
                            [ "Not enough:", show s
                            , ", have", show (bagLookup s (gameResources g))
                            , ", need", show n ]

         runInteract k g { gameResources = s1 }

    ChooseNum xs k  ->
      case partitionEithers (map attempt opts) of
        (errs,[])     -> Left $ unlines ("Nothing would work" : errs)
        (_,[(_,_,o)]) -> return o
        (_,ys)        -> return $ NumChoices g $ Map.fromList
                            [ (e,(d,o)) | (d,e,o) <- ys ]
      where
      opts           = zip xs [ 0 .. ]
      attempt (d,el) = do o <- runInteract (k el) g
                          return (d,el,o)

runIM :: IM () -> Interact
runIM (IM k) = k (\_ -> Done)


--------------------------------------------------------------------------------

move :: Addr -> IM ()
move a =
  do g <- getGame
     let player = gameLocation g
         land0  = gameLand g
     unless (a `Set.member` neighboursUpTo 1 player) backtrack
     case getHexInfo a land0 of

       -- Try exploring
       Nothing ->
         do spend 2 Movement
            case exploreAt player (addrGlobal a) land0 of
              Ok newLand -> updGame (\g1 -> g1 { gameLand = newLand })
              Failed _   -> backtrack

       Just i ->
         case Map.lookup (hexTerrain (hexLandInfo i)) (gameTerrainCosts g) of
           Nothing -> backtrack
           Just n  -> do spend n Movement
                         updGame (\g1 -> g1 { gameLocation = a })

summon :: Spirit -> IM ()
summon p =
  do spend 1 Summon
     spend 1 (Spirit p IsAvailable)
     gain  1 (Spirit p WasSummoned)
     e <- case p of
            Death     -> backtrack
            Birth     -> chooseElement
            Element e -> return e
     gain 1 (Summoned e)

summonFriendly :: Element -> IM ()
summonFriendly e =
  do spend 1 (Friendly e)
     gain  1 (Summoned e)

spendAction :: IM Action
spendAction =
  do a <- chooseAction
     spend 1 (Action a)
     gain  1 (UsedAction a)
     return a

useBasic :: Action -> IM ()
useBasic a =
  do spend 1 (Action a)
     gain  1 (UsedAction a)
     actDo (actBase a)

useAdvanced :: Action -> IM ()
useAdvanced a =
  do spend 1 (Action a)
     gain  1 (UsedAction a)
     spend 1 (Summoned (actElement a))
     actDo (actAdvanced a)




--------------------------------------------------------------------------------


instance Enumerate Action where
  enumerate = allActions

data Description = Text Text
                 | Image Text
                 | ActionD Action
                 | GlueHor Description Description
                 | GlueVer Description Description

infixr 2 <+>, </>
infixr 1 $$

(<+>) :: Description -> Description -> Description
x <+> y = GlueHor x y -- (GlueHor (Text " ") y)

(</>) :: Description -> Description -> Description
x </> y = x <+> Text "/" <+> y

($$) :: Description -> Description -> Description
($$)  = GlueVer




imgElement :: Element -> Description
imgElement e =
  case e of
    Water -> Image "water"
    Fire  -> Image "fire"
    Earth -> Image "earth"
    Air   -> Image "air"

imgStored :: Description
imgStored = Image "container_solid"

imgSpirit :: Spirit -> Description
imgSpirit s =
  case s of
    Death -> Image "skull"
    Birth -> Image "elements"
    Element e -> imgElement e

imgBlock :: Description
imgBlock    = Image "block"

imgAttack  :: Description
imgAttack   = Image "attack"

imgCurrency :: Description
imgCurrency = Image "currency"

imgMove :: Description
imgMove     = Image "movement"

imgAction :: Description
imgAction   = Image "action"

imgAdvancedAction :: Description
imgAdvancedAction = Image "advanced_action"

imgHeal :: Description
imgHeal     = Image "heal"

imgSummon :: Description
imgSummon   = Image "well"

imgFlaw :: Description
imgFlaw = Image "devil"

allActions :: [Action]
allActions = concat
  [
    -- Air
    replicate 1
    Act { actName      = "Play"
        , actElement   = Air
        , actBase      = part (Text "1" <+> imgSummon)
                       $ gain 1 Summon

        , actAdvanced  = part (Text "Set the type of a spirit;" $$
                               Text "2 elements of that type.")
                       $ do x <- chooseSpirit
                            spend 1 (Spirit x IsAvailable)
                            y <- chooseElement
                            gain 2 (Summoned y)
                            gain 1 (Spirit (Element y) WasTransformed)
        }

  , replicate 1
    Act { actName     = "Barter"
        , actElement  = Air
        , actBase     = part (Text "2" <+> imgCurrency)
                      $ gain 2 Currency
        , actAdvanced = part (Text "4" <+> imgCurrency)
                      $ gain 4 Currency
        }

  , replicate 2
    Act { actName     = "Fly"
        , actElement  = Air
        , actBase     = part (Text "2" <+> imgMove)
                      $ gain 2 Movement
        , actAdvanced = part (Text "4" <+> imgAttack)
                      $ gain 4 Attack
        }

  -- Water
  , replicate 1
    Act { actName     = "Store"
        , actElement  = Water
        , actBase     = part (Image "elements" <+> Text "to"
                                               <+> imgStored)
                      $ do e <- chooseElement
                           spend 1 (Summoned e)
                           gain  1 (Friendly e)
        , actAdvanced = part (Text "1" <+> imgStored)
                      $ do e <- chooseElement
                           gain 1 (Friendly e)
        }

  , replicate 1
    Act { actName     = "Block"
        , actElement  = Water
        , actBase     = part (Text "2" $$ imgBlock </> imgAttack)
                      $ choices [ (imgBlock,  gain 2 Block)
                                , (imgAttack, gain 2 Attack)
                                ]
        , actAdvanced = part (Text "5" <+> imgBlock)
                      $ gain 5 Block
        }

  , replicate 2
    Act { actName     = "Swim"
        , actElement  = Water
        , actBase     = part (Text "2" <+> imgMove)
                      $ gain 2 Movement
        , actAdvanced = part (Text "4" <+> imgMove)
                      $ gain 4 Movement
        }

  -- Earth
 , replicate 2
    Act { actName      = "Walk"
       , actElement   = Earth
       , actBase      = part (Text "2" <+> imgMove)
                      $ gain 2 Movement
       , actAdvanced  = part (Text "4" <+> imgMove)
                      $ gain 4 Movement
       }

  , replicate 1
    Act { actName     = "Think"
        , actElement  = Earth
        , actBase     = part (Text "1" $$ imgHeal </> imgAction)
                      $ choices [ (imgHeal, gain 1 Heal)
                                , (imgAction, extraAction) ]
        , actAdvanced = part (Text "2" $$ imgHeal </> imgAction)
                      $ choices [ (imgHeal, gain 2 Heal)
                                , (imgAction, extraAction >> extraAction) ]
        }

  , replicate 1
    Act { actName     = "Search"
        , actElement  = Earth
        , actBase     = part (Text "1" $$
                               imgElement Water </> imgElement Air </>
                               imgElement Fire)
                      $ do e <- chooseElement
                           when (e == Earth) backtrack
                           gain 1 (Summoned e)
        , actAdvanced = part (Text "Free" <+> imgAdvancedAction $$
                               Text "+2" <+>
                                  imgMove <+> imgCurrency <+> imgBlock
                                  <+> imgAttack)
                      $ do a <- spendAction
                           doInteract $ boost 2 $ runIM $ actDo $ actAdvanced a
        }

  -- Fire
  , replicate 2
    Act { actName     = "Attack"
        , actElement  = Fire
        , actBase     = part (Text "2" $$ imgBlock </> imgAttack)
                      $ choices [ (imgBlock,  gain 2 Block)
                                , (imgAttack, gain 2 Attack) ]
        , actAdvanced = part (Text "4" <+> imgAttack)
                      $ gain 4 Attack
        }

  , replicate 1
    Act { actName     = "Steal"
        , actElement  = Fire
        , actBase     = part (Text "2" <+> imgCurrency)
                      $ gain 2 Currency
        , actAdvanced = part (Text "5" <+> imgCurrency $$
                                Text "1" <+> imgFlaw )
                      $ do gain 5 Currency
                           gain 1 CharacterFlaw
        }

  , let opts n = choices [ (imgMove,      gain n Movement)
                         , (imgCurrency,  gain n Currency)
                         , (imgBlock,     gain n Block)
                         , (imgAttack,    gain n Attack)
                         ]
    in replicate 1
    Act { actName     = "Improve"
        , actElement  = Fire
        , actBase     = part (imgAction <+> Text "to 3" $$
                            imgMove </> imgCurrency </> imgBlock </> imgAttack)
                      $ spendAction >> opts 3

        , actAdvanced = part (imgAction <+> Text "to 5" $$
                            imgMove </> imgCurrency </> imgBlock </> imgAttack)
                      $ spendAction >> opts 5
        }
  ]

  where
  part a b = ActPart { actDo = b, actHelp = a }


boost :: Int -> Interact -> Interact
boost b i0 =
  case i0 of
    ChooseNum n   k -> ChooseNum n   (boost b . k)
    ExtraAction   k -> ExtraAction (boost b k)
    Spend n k i     -> Spend n k (boost b i)
    Gain n s i
      | s `elem` [ Movement, Block, Attack, Currency ]
                    -> Gain (n+b) s (boost b i)
      | otherwise   -> Gain n s (boost b i)

    GetGame k       -> GetGame (boost b . k)
    UpdGame g k     -> UpdGame g (boost b k)


    Fail            -> Fail
    Done            -> Done



