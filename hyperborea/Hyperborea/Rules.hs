{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Hyperborea.Rules
  ( Factory
  , factoryEmpty
  , factoryAddGroup
  , factoryChangeSize
  , factoryExtendSource
  , factoryUpdateLimit
  , factoryDestroyInput
  , factoryDestroyDiscarded
  , factoryDestroyFromPool

  , factoryActivate
  , factoryApply
  , factoryUse
  , factoryProduce
  , factoryForceReset
  , factoryEndPeriod
  , factoryRestock
  , factoryTimeForReset
  , factoryReset
  , Rule(..)
  , RuleYield(..)
  , Inputs(..)

  , RuleGroup
  , ruleGroup

  , Raw(..)
  , Material(..)
  , Action(..)
  , AdjEffect(..)
  , LongTermAction(..)
  , ImmediateAction(..)
  , Upgrade(..)

  -- XXX
  , upgradeProduce

  ) where

import Control.Monad(guard)
import Data.Text(Text)
import Data.List(findIndex,unfoldr)
import Data.Maybe(isJust,mapMaybe)
import Util.Bag
import Util.Perhaps
import Util.Random

import Util.JSON


data Raw      = Green | Red | Magenta | Orange | Yellow | Blue
                deriving (Eq,Ord)

data Material = Waste | Raw Raw
                deriving (Eq,Ord)



data Inputs   = Inputs { inputsWild     :: Int
                         -- ^ Any raw material
                       , inputsMaterial :: Bag Material
                         -- ^ A specific set of raw materials
                       , inputsSacrifice :: Int
                         -- ^ An active player
                       }

inputsAreEmpty :: Inputs -> Bool
inputsAreEmpty i = inputsWild i == 0 && bagIsEmpty (inputsMaterial i)

inputsRemoveWild :: Inputs -> Maybe Inputs
inputsRemoveWild Inputs { .. } =
  do guard (inputsWild > 0)
     return Inputs { inputsWild = inputsWild - 1, .. }

inputsRemoveMaterial :: Material -> Inputs -> Maybe Inputs
inputsRemoveMaterial m Inputs { .. } =
  do im <- bagRemove 1 m inputsMaterial
     return Inputs { inputsMaterial = im, .. }

--------------------------------------------------------------------------------

data Action = Move   | Fly
            | Attack | RangedAttack
            | Fortify
            | ProgressDifferent
            | Progress1 | Progress2 | Progress3 | Progress4
            | Buy
            | Spawn | Clone
            | GainWild | ChangeWild
            | Gem
            | Draw
            | Espionage
            deriving (Eq,Ord,Show,Bounded,Enum)

data AdjEffect  = LooseGem
                | GainAction Action
                  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------

data LongTermAction   = WhenProduce Action Upgrade
                      | AtStart (Bag Action)

data Upgrade          = ConvertTo Action
                      | Generate (Bag Action)

upgradeProduce :: [LongTermAction] -> Bag Action -> Bag Action
upgradeProduce acts = upgradeWith convert  convertors
                    . upgradeWith generate generators
  where
  upgradeWith f us as = case unfoldr (upgradeStep f) (us,as) of
                          [] -> as
                          xs -> last xs

  upgradeStep f (us,as) =
    case foldr (upgarde f) (False, [], as) us of
      (ch,later,as') -> if ch then Just (as',(later,as')) else Nothing

  upgarde f (a,b) (changes, later, as) =
    let p = bagLookup a as
    in if p > 0 then (True, later, f p a b as) else (changes, (a,b):later, as)


  convert n a b as   = bagAdd n b (bagRemoveAll a as)
  generate _ _ bs as = bagUnion bs as

  (convertors, generators) = foldr classify ([],[]) acts

  classify p (conv,gen) =
    case p of
      AtStart _       -> (conv,gen)
      WhenProduce a u ->
        case u of
          ConvertTo b   -> ((a,b) : conv, gen)
          Generate bs   -> (conv, (a,bs) : gen)



--------------------------------------------------------------------------------


data Rule     = Rule { ruleName       :: Text
                     , ruleInputs     :: Inputs
                     , ruleProduces   :: RuleYield
                     }

data RuleYield  = Immediate [ ImmediateAction ] -- ^ Pick one of these actions
                | LongTerm  LongTermAction      -- ^ A long-term benefit

data ImmediateAction = ImmediateAction
  { playerActions   :: Bag Action     -- ^ Effects for player
  , adjacentActions :: Bag AdjEffect  -- ^ Effects on neighbours
  }



data ActiveRule = ActiveRule
  { activeOriginal  :: Rule           -- ^ The original rule
  , activeNeed      :: Inputs         -- ^ What inputs are still missing
  , activeHave      :: Bag Material   -- ^ The materials used in the instance
  , activeFired     :: Bool           -- ^ Did this rule generate its produce
  , activeReset     :: Bool           -- ^ Should we reset this rule.
                                      -- Allows fro reseting long-term actions.
  }

activateRule :: Rule -> ActiveRule
activateRule r = ActiveRule { activeOriginal  = r
                            , activeNeed      = ruleInputs r
                            , activeHave      = bagEmpty
                            , activeFired     = False
                            , activeReset     = case ruleProduces r of
                                                  Immediate _ -> True
                                                  LongTerm _  -> False
                            }

activeRuleProduce :: Int -> ActiveRule -> Perhaps (ImmediateAction, ActiveRule)
activeRuleProduce v ActiveRule { .. }
  | not (inputsAreEmpty activeNeed) = Failed "Not yet ready to produce."
  | activeFired                     = Failed "We already produced."

  | Immediate opts <- ruleProduces activeOriginal =
    case lookup v (zip [ 0 .. ] opts) of
      Just as -> return (as, ActiveRule { activeFired = True, .. })
      Nothing -> Failed "We don't know this variant."
  | LongTerm _ <- ruleProduces activeOriginal =
    Failed "Long-term actions are used automaitcally."

activeRuleLongTermReady :: ActiveRule -> Maybe LongTermAction
activeRuleLongTermReady ActiveRule { .. }
  | inputsAreEmpty activeNeed
  , LongTerm a <- ruleProduces activeOriginal = return a
  | otherwise                                 = Nothing


activeRuleApply :: Material -> ActiveRule -> Perhaps ActiveRule
activeRuleApply m ActiveRule { .. } =
  case inputsRemoveMaterial m activeNeed of
    Just i  -> ok i
    Nothing ->
      case m of
        Waste -> noSlot
        Raw _ ->
          case inputsRemoveWild activeNeed of
            Just i  -> ok i
            Nothing -> noSlot
  where
  noSlot  = Failed "No slot accepts this material."
  ok i    = return ActiveRule { activeHave = bagAdd 1 m activeHave
                              , activeNeed = i
                              , .. }

activeRuleDestroyInput :: Material -> ActiveRule -> Perhaps ActiveRule
activeRuleDestroyInput m ActiveRule { .. } =
  case bagRemove 1 m activeHave of
    Nothing -> Failed "Cannot destroy this material."
    Just b  -> Ok ActiveRule { activeHave  = b
                             , activeFired = False
                             , activeNeed  = newNeed
                             , .. }

  where
  wildFilled = inputsWild (ruleInputs activeOriginal) - inputsWild activeNeed
  newNeed =
    let Inputs { .. } = activeNeed
    in if wildFilled > 0
         then Inputs { inputsWild = 1 + inputsWild, .. }
         else Inputs { inputsMaterial = bagAdd 1 m inputsMaterial, .. }


activeRuleForceReset :: Bool -> ActiveRule -> Perhaps ActiveRule
activeRuleForceReset r ActiveRule { .. }
  | r = reset
  | LongTerm _ <- ruleProduces activeOriginal = reset
  | otherwise = Failed "Cannot persist resets."
  where reset = return ActiveRule { activeReset = r, .. }

-- | We do this when this rule survives a reset
activeRuleRestart :: ActiveRule -> ActiveRule
activeRuleRestart ActiveRule { .. } = ActiveRule { activeFired = False, .. }

activeRuleResourceNum :: ActiveRule -> Int
activeRuleResourceNum ActiveRule { .. }
  | activeReset = bagSize activeHave
  | otherwise   = 0



--------------------------------------------------------------------------------

-- | A collection of rules, only one of which may be used at a time.
data RuleGroup = RuleGroup
  { rules       :: [ Rule ]           -- ^ All alternatives
  , rulesActive :: Maybe ActiveRule   -- ^ The rule that is currently active
  , rulesVP     :: !Int               -- ^ Victory points
  }

-- | Construct a new group using worth some victory points, and some rules.
ruleGroup :: Int -> [Rule] -> RuleGroup
ruleGroup rulesVP rules = RuleGroup { rulesActive = Nothing, .. }

-- | Does nothing if the group is already active.
chooseActive :: Int -> RuleGroup -> Perhaps RuleGroup
chooseActive n RuleGroup { .. } =
  case rulesActive of
    Just _  -> return RuleGroup { .. }
    Nothing ->
      case splitAt n rules of
        (_,r:_) -> Ok RuleGroup { rulesActive = Just (activateRule r), .. }
        _       -> Failed "There is no such rule."




updateActive :: (ActiveRule -> Perhaps (a,ActiveRule)) ->
                (RuleGroup -> Perhaps (a,RuleGroup))
updateActive f RuleGroup { .. } =
  case rulesActive of
    Just r  -> ok r
    Nothing -> case rules of
                 [ r ] -> ok (activateRule r)
                 _     -> Failed "This groups is not yet activated."
  where ok r = do (a,r1) <- f r
                  return (a,RuleGroup { rulesActive = Just r1, .. })

updateActive_ :: (ActiveRule -> Perhaps ActiveRule) ->
                 (RuleGroup -> Perhaps RuleGroup)
updateActive_ = discarding updateActive

discarding :: ((a -> Perhaps ((),b)) -> (p -> Perhaps ((),q))) ->
              (a -> Perhaps b) -> (p -> Perhaps q)
discarding op f g = snd <$> op f' g
  where f' r = (\x -> ((),x)) <$> f r

ruleGroupApply :: Material -> RuleGroup -> Perhaps RuleGroup
ruleGroupApply m = updateActive_ (activeRuleApply m)

ruleGroupDestroyInput :: Material -> RuleGroup -> Perhaps RuleGroup
ruleGroupDestroyInput m = updateActive_ (activeRuleDestroyInput m)

ruleGroupProduce :: Int -> RuleGroup -> Perhaps (ImmediateAction, RuleGroup)
ruleGroupProduce v = updateActive (activeRuleProduce v)

ruleGroupLongTerm :: RuleGroup -> Maybe LongTermAction
ruleGroupLongTerm RuleGroup { .. } = activeRuleLongTermReady =<< rulesActive

ruleGroupForceReset :: Bool -> RuleGroup -> Perhaps RuleGroup
ruleGroupForceReset r = updateActive_ (activeRuleForceReset r)

ruleGroupReset :: RuleGroup -> (Bag Material, RuleGroup)
ruleGroupReset RuleGroup { .. } =
  case rulesActive of
    Just a
      | activeReset a ->
        (activeHave a, RuleGroup { rulesActive = Nothing, .. })

      | otherwise ->
        (bagEmpty, RuleGroup { rulesActive = Just (activeRuleRestart a), .. })

    Nothing -> (bagEmpty, RuleGroup { .. })

ruleGroupResourceNum :: RuleGroup -> Int
ruleGroupResourceNum RuleGroup { .. } =
  case rulesActive of
    Just a  -> activeRuleResourceNum a
    Nothing -> 0

--------------------------------------------------------------------------------

data Factory = Factory
  { factoryGroups     :: ![RuleGroup]
  , factoryGroupLimit :: !(Maybe Int)

  , factoryRandom     :: !StdGen
  , factorySource     :: ![ Material ]
  , factoryDiscarded  :: !(Bag Material)

  , factoryPoolSize   :: !Int               -- ^ How many raw to use
  , factoryPool       :: ![Material]

  , factoryProduced   :: !(Bag Action)
  }

factoryEmpty :: Gen Factory
factoryEmpty =
  do factoryRandom <- randStdGen
     return Factory
             { factoryGroups      = []
             , factoryGroupLimit  = Nothing
             , factorySource      = []
             , factoryDiscarded   = bagEmpty
             , factoryPoolSize    = 0
             , factoryPool        = []
             , factoryProduced    = bagEmpty
             , ..
             }

factoryAddGroup :: RuleGroup -> Factory -> Perhaps Factory
factoryAddGroup g Factory { .. }
  | Just n <- factoryGroupLimit, length factoryGroups >= n + 1 =
    Failed "No more space for groups."
  | otherwise =
    Ok Factory { factoryGroups = g : factoryGroups, .. }

factoryUpdateLimit :: (Maybe Int -> Maybe Int) -> Factory -> Factory
factoryUpdateLimit f Factory { .. } =
  Factory { factoryGroupLimit = f factoryGroupLimit, .. }

factoryChangeSize :: Int -> Factory -> Factory
factoryChangeSize d Factory { .. } =
  Factory { factoryPoolSize = max 0 (factoryPoolSize + d), .. }

factoryExtendSource :: Material -> Factory -> Factory
factoryExtendSource m Factory { .. } =
  case factorySource of
    [] -> Factory { factorySource = m : factorySource, .. }
    _  -> genRandFun factoryRandom $
            do x <- randInRange 0 (length factorySource - 1)
               let (as,bs) = splitAt x factorySource
               return $ \newGen ->
                  Factory { factorySource = as ++ m : bs
                          , factoryRandom = newGen
                          , ..
                          }

updateRuleGroup ::
  Int -> (RuleGroup -> Perhaps (a,RuleGroup)) -> Factory -> Perhaps (a,Factory)
updateRuleGroup n f Factory { .. } =
  case splitAt n factoryGroups of
    (as,b:bs) ->
      do (x,b1) <- f b
         return (x, Factory { factoryGroups = as ++ b1 : bs, .. })
    (_,[]) -> Failed "There is no such group."


updateRuleGroup_ :: Int -> (RuleGroup -> Perhaps RuleGroup) ->
                           (Factory -> Perhaps Factory)
updateRuleGroup_ n = discarding (updateRuleGroup n)


-- | Select a rule to be working on.
factoryActivate :: Int {- variant -} -> Int {-^ group -} ->
                   Factory -> Perhaps Factory
factoryActivate v g = updateRuleGroup_ g (chooseActive v)

-- | Add a resources to a rule.
factoryApply ::
  Int {-^ resource -} -> Int {-^ group -} -> Factory -> Perhaps Factory
factoryApply m g Factory { .. } =
  case splitAt m factoryPool of
    (as,b:bs) -> updateRuleGroup_ g (ruleGroupApply b)
                    Factory { factoryPool = as ++ bs, .. }
    _ -> Failed "This material is not avilable."

-- | Produce using a rule.
factoryProduce :: Int {-^ variant -} -> Int {-^ group -} ->
              Factory -> Perhaps Factory
factoryProduce v g f =
  do (ImmediateAction as adj, Factory { .. }) <-
                                updateRuleGroup g (ruleGroupProduce v) f
     -- XXX: apply `adj` effects
     return Factory { factoryProduced = bagUnion as factoryProduced, .. }

-- | Remove a resource from a rule.
factoryDestroyInput :: Material -> Int -> Factory -> Perhaps Factory
factoryDestroyInput m g f = updateRuleGroup_ g (ruleGroupDestroyInput m) f

-- | Remove a resource from the discarded area.
factoryDestroyDiscarded :: Material -> Factory -> Perhaps Factory
factoryDestroyDiscarded m Factory { .. } =
  case bagRemove 1 m factoryDiscarded of
    Just b -> return Factory { factoryDiscarded = b, .. }
    Nothing -> Failed "No such discarded"

-- | Remove a resource from the ready-to-use area.
factoryDestroyFromPool :: Material -> Factory -> Perhaps Factory
factoryDestroyFromPool m Factory { .. } =
  case findIndex (== m) factoryPool of
    Nothing -> Failed "No such material in pool"
    Just i  -> let (as,_:bs) = splitAt i factoryPool
               in return Factory { factoryPool = as ++ bs
                                 , factoryPoolSize = factoryPoolSize - 1, .. }


-- | Use an action that was produced.
factoryUse :: Int -> Action -> Factory -> Perhaps Factory
factoryUse n a Factory { .. } =
  perhaps "Not enough actions." $
    do b1 <- bagRemove n a factoryProduced
       return Factory { factoryProduced = b1, .. }

-- | Get the currently active long-term benefits for this factory.
factoryLongTerm :: Factory -> [LongTermAction]
factoryLongTerm Factory { .. } = mapMaybe ruleGroupLongTerm factoryGroups


factoryForceReset :: Bool -> Int {-^ group -} -> Factory -> Perhaps Factory
factoryForceReset r n = updateRuleGroup_ n (ruleGroupForceReset r)

factoryEndPeriod :: Factory -> Factory
factoryEndPeriod Factory { .. } =
  Factory { factoryPool       = []
          , factoryDiscarded  = bagUnion (bagFromList factoryPool)
                                         factoryDiscarded
          , factoryProduced   = bagEmpty
          , ..
          }

factoryRestock :: Factory -> Factory
factoryRestock Factory { .. } =
  Factory { factoryPool   = factoryPool ++ new
          , factorySource = rest
          , .. }
  where
  have = length factoryPool
  need = max 0 (factoryPoolSize - have)
  (new,rest) = splitAt need factorySource

factoryTimeForReset :: Factory -> Bool
factoryTimeForReset Factory { .. } = null factorySource

factoryReset :: Factory -> Factory
factoryReset Factory { .. } =
  genRandFun factoryRandom $
    do let (rs,gs)  = unzip (map ruleGroupReset factoryGroups)
           newRs    = foldr bagUnion factoryDiscarded rs
       newSrc <- shuffle (factorySource ++ bagToList newRs)
       return $ \newRand -> Factory { factoryDiscarded = bagEmpty
                                    , factorySource    = newSrc
                                    , factoryGroups    = gs
                                    , factoryRandom    = newRand
                                    , factoryProduced  = bagEmpty
                                    , ..
                                    }

-- | Total number of materials in the factory.
factoryResourceNum :: Factory -> Int
factoryResourceNum Factory { .. } =
  sum ( length factoryPool
      : length factorySource
      : bagSize factoryDiscarded
      : map ruleGroupResourceNum factoryGroups)


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

instance Export Factory where
  toJS Factory { .. } =
    object [ "groupLimit"   .= factoryGroupLimit
           , "groups"       .= factoryGroups
           , "source"       .= length factorySource
           , "sourceSize"   .= factoryResourceNum Factory { .. }
           , "poolSize"     .= factoryPoolSize
           , "pool"         .= factoryPool
           , "discarded"    .= bagToList factoryDiscarded
           , "produced"     .= actionsToJS factoryProduced
           ]

actionToText :: Action -> Text
actionToText a =
  case a of
    Move              -> "move"
    Fly               -> "fly"
    Attack            -> "attack"
    RangedAttack      -> "ranged_attack"
    Fortify           -> "fortify"
    ProgressDifferent -> "progress_different" -- XXX
    Buy               -> "buy"

actionsToJS :: Bag Action -> Value
actionsToJS = object . map mk . bagToListGrouped
  where mk (a,n) = actionToText a .= n

instance Export Material where
  toJS m = case m of
             Waste -> toJS ("waste" :: Text)
             Raw r -> toJS r

instance Export Raw where
  toJS r = toJS $ case r of
                    Green   -> "A" :: Text
                    Red     -> "B"
                    Magenta -> "C"
                    Orange  -> "D"
                    Yellow  -> "E"
                    Blue    -> "F"

instance Export RuleGroup where
  toJS RuleGroup { .. } =
    let actName = fmap (ruleName . activeOriginal) rulesActive
    in object [ "rules" .= [ if Just (ruleName r) == actName
                             then toJS rulesActive else toJS r | r <- rules ]
              , "activated" .= isJust rulesActive
              ]

instance Export ActiveRule where
  toJS ActiveRule { activeOriginal = Rule { .. }, .. } =
    object $ ruleFields Rule { ruleInputs   = activeNeed, .. }
              ++ [ "have"      .= bagToList activeHave
                 , "fired"     .= activeFired
                 , "willreset" .= activeReset
                 ]

instance Export Rule where
  toJS = object . ruleFields

ruleFields :: Rule -> [(Text,Value)]
ruleFields Rule { .. } =
  [ "name"       .= ruleName
  , "inputs"     .= ruleInputs
  , "produce"    .= ruleProduces
  ]

instance Export RuleYield where
  toJS y =
    case y of
      Immediate as ->
        object [ jsTag "immediate", "outputs" .= as ]
      LongTerm a ->
        object [ jsTag "long_term", "outputs" .= a ]


instance Export ImmediateAction where
  toJS _ = toJS ("XXX: IMMEDIATE_ACTION " :: Text)

instance Export LongTermAction where
  toJS _ = toJS ("XXX: LONG_TERM_ACTION" :: Text)

instance Export Inputs where
  toJS Inputs { .. } =
    toJS (replicate inputsWild wild ++ map toJS (bagToList inputsMaterial))
    where wild = toJS ("wild" :: Text)

