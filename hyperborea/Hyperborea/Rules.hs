{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Hyperborea.Rules
  ( Raw(..)
  , Material(..)
  , AnyMaterial(..)
  , Input(..)
  , Action(..)
  , AdjEffect(..)
  , LongTermAction(..)
  , Upgrade(..)
  , Rule(..)
  , RuleYield(..)
  , ImmediateAction(..)

  , RuleGroup
  , ruleGroup
  , ruleGroupActiveRule
  , ruleGroupRules
  , ruleGroupVPs

  , ActiveRule(..)

  , Factory(..)
  , factoryEmpty
  , factoryChangeSize
  , factoryAddGroup
  , factoryExtendSource
  , factoryUpdateLimit

  , factoryRestock
  , factoryEndPeriod
  , factoryForceReset
  , factoryReset
  , factoryTimeForReset

  , factoryActivate

  , factoryUseMaterial
  , factoryRecall
  , factoryDestroyFromPool
  , factoryDestroyDiscarded

  , factoryResourceNum

  , factoryProduce

  , factoryUse

  ) where

import Data.Text(Text)
import Data.List(findIndex,unfoldr)
import Data.Maybe(mapMaybe)
import Util.Bag
import Util.Perhaps
import Util.Random



data Raw            = Green | Red | Magenta | Orange | Yellow | Blue
                      deriving (Eq,Ord)

data Material       = Waste | Raw Raw
                      deriving (Eq,Ord)

data AnyMaterial    = AnyRaw | Material Material
                      deriving (Eq,Ord)

data Input          = Recall              -- ^ Recall an active avatar
                    | Use     AnyMaterial -- ^ Use some material
                    | Discard AnyMaterial -- ^ Discard a material
                      deriving (Eq,Ord)



--------------------------------------------------------------------------------

data Action         = Move   | Fly
                    | Attack | RangedAttack
                    | Fortify
                    | ProgressDifferent | Progress1 | Progress2 | Progress3
                    | Buy
                    | Spawn | Clone
                    | GainAnyRaw | ChangeAnyRaw
                    | Gem
                    | Draw | Restore
                    | Espionage
                      deriving (Eq,Ord,Show,Bounded,Enum)

data AdjEffect      = LooseGem
                    | GainAction Action
                      deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------

data LongTermAction = WhenProduce Action Upgrade
                    | AtStart (Bag Action)

data Upgrade        = ConvertTo Action
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
                     , ruleInputs     :: Bag Input
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
  , activeNeed      :: Bag Input      -- ^ What inputs are still missing
  , activeHave      :: Bag Material
    -- Materials used in the instance, which will be reused.
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
  | not (bagIsEmpty activeNeed)   = Failed "Not yet ready to produce."
  | activeFired                   = Failed "We already produced."
  | otherwise =
    case ruleProduces activeOriginal of
      Immediate opts ->
        case lookup v (zip [ 0 .. ] opts) of
          Just as -> return (as, ActiveRule { activeFired = True, .. })
          Nothing -> Failed "We don't know this variant."
      LongTerm _ ->
        Failed "Long-term actions are used automaitcally."


activeRuleLongTermReady :: ActiveRule -> Maybe LongTermAction
activeRuleLongTermReady ActiveRule { .. }
  | bagIsEmpty activeNeed
  , LongTerm a <- ruleProduces activeOriginal = return a
  | otherwise                                 = Nothing


activeRuleApply :: Input -> ActiveRule -> Perhaps ActiveRule
activeRuleApply m ActiveRule { .. }
  | Use AnyRaw <- m     = wildErr
  | Discard AnyRaw <- m = wildErr
  | otherwise =
    case bagRemove 1 m activeNeed of
      Just i  -> ok i
      _ | Just w <- asAnyRaw, Just i <- bagRemove 1 w activeNeed -> ok i

      _ -> Failed "No slot accepts this material."
  where
  wildErr   = Failed "Only concrete materials may be used."
  asAnyRaw  = case m of
                Use (Material (Raw _))      -> Just (Use AnyRaw)
                Discard (Material (Raw _))  -> Just (Discard AnyRaw)
                _                           -> Nothing

  ok i =
    case m of
      Use (Material x) ->
        return ActiveRule { activeHave = bagAdd 1 x activeHave
                          , activeNeed = i
                          , .. }
      _ -> return ActiveRule { activeNeed = i, .. }


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

ruleGroupApply :: Input -> RuleGroup -> Perhaps RuleGroup
ruleGroupApply m = updateActive_ (activeRuleApply m)

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

ruleGroupActiveRule :: RuleGroup -> Maybe ActiveRule
ruleGroupActiveRule RuleGroup { .. } = rulesActive

ruleGroupRules :: RuleGroup -> [Rule]
ruleGroupRules RuleGroup { .. } = rules

ruleGroupVPs :: RuleGroup -> Int
ruleGroupVPs RuleGroup { .. } = rulesVP

--------------------------------------------------------------------------------

data Factory = Factory
  { factoryGroups     :: ![RuleGroup]
  , factoryGroupLimit :: !(Maybe Int)

  , factoryRandom     :: !StdGen
  , factorySource     :: ![ Material ]
  , factoryDiscarded  :: !(Bag Material)

  , factoryPoolSize   :: !Int  -- ^ How many to draw by default (e.g. 3)
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



--------------------------------------------------------------------------------
-- Satisfying rule requirements

-- | Add a resources to a rule.
factoryUseMaterial ::
  Int {-^ resource -} -> Int {-^ group -} -> Factory -> Perhaps Factory
factoryUseMaterial m g Factory { .. } =
  case splitAt m factoryPool of
    (as,b:bs) -> factoryApplyInput (Use (Material b)) g
                                   Factory { factoryPool = as ++ bs, .. }
    _         -> Failed "This material is not avilable."

-- | Use a material from the pool to satisfy a rule.
factoryApplyInput :: Input -> Int {-^ group -} -> Factory -> Perhaps Factory
factoryApplyInput i g = updateRuleGroup_ g (ruleGroupApply i)

-- | Recall a unit to satisfy a rule.
factoryRecall :: Int {-^ group -} -> Factory -> Perhaps Factory
factoryRecall = factoryApplyInput Recall

-- | Discard a resource from the discarded area to satisfy a rule.
factoryDestroyDiscarded :: Material -> Int -> Factory -> Perhaps Factory
factoryDestroyDiscarded m g Factory { .. } =
  case bagRemove 1 m factoryDiscarded of
    Just b  -> factoryApplyInput (Discard (Material m)) g
                                 Factory { factoryDiscarded = b, .. }
    Nothing -> Failed "No such discarded"

-- | Discard a resource from the ready-to-use area, to satisfy a rule.
factoryDestroyFromPool :: Material -> Int -> Factory -> Perhaps Factory
factoryDestroyFromPool m g Factory { .. } =
  case findIndex (== m) factoryPool of
    Just i -> factoryApplyInput (Discard (Material m)) g
                                Factory { factoryPool = as ++ bs, .. }
      where (as,_:bs) = splitAt i factoryPool
    Nothing -> Failed "No such material in pool"





--------------------------------------------------------------------------------

-- | Produce using a rule.
factoryProduce :: Int {-^ variant -} -> Int {-^ group -} ->
              Factory -> Perhaps Factory
factoryProduce v g f =
  do (ImmediateAction as adj, Factory { .. }) <-
                                updateRuleGroup g (ruleGroupProduce v) f
     let bonuses = factoryLongTerm f
         bs      = upgradeProduce bonuses as

     -- XXX: apply `adj` effects
     return Factory { factoryProduced = bagUnion bs factoryProduced, .. }



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



