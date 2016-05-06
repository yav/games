{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}
module Hyperborea.Protocol where

import Data.Text(Text)
import Data.Maybe(isJust)
import Util.Bag

import qualified Util.JSON as JS

import Hyperborea.Rules

class Export t where
  toJS :: t -> JS.Value

(.=) :: Export a => Text -> a -> (Text,JS.Value)
x .= y = (x,toJS y)

jsTag :: Text -> (Text,JS.Value)
jsTag x = "tag" .= x

instance Export JS.Value              where toJS = id
instance Export Int                   where toJS = JS.int
instance Export Bool                  where toJS = JS.bool
instance Export Text                  where toJS = JS.text
instance Export a => Export [a]       where toJS = JS.list toJS
instance Export a => Export (Maybe a) where toJS = JS.maybe toJS



instance Export Factory where
  toJS Factory { .. } =
    JS.object [ "groupLimit"   .= factoryGroupLimit
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
    ProgressDifferent -> "progress_different"
    Progress1         -> "progress1"
    Progress2         -> "progress2"
    Progress3         -> "progress3"
    Buy               -> "buy"
    Spawn             -> "spawn"
    Clone             -> "clone"
    GainAnyRaw        -> "gain_any"
    ChangeAnyRaw      -> "change_any"
    Gem               -> "gem"
    Draw              -> "draw"
    Restore           -> "restore"
    Espionage         -> "spy"

instance Export Action where
  toJS = JS.text . actionToText



actionsToJS :: Bag Action -> JS.Value
actionsToJS = JS.object . map mk . bagToListGrouped
  where mk (a,n) = actionToText a .= n

instance Export Material where
  toJS m = case m of
             Waste -> toJS ("waste" :: Text)
             Raw r -> toJS r

instance Export AnyMaterial where
  toJS m = case m of
             AnyRaw     -> JS.text "any"
             Material a -> toJS a

instance Export Raw where
  toJS r = toJS $ case r of
                    Green   -> "A" :: Text
                    Red     -> "B"
                    Magenta -> "C"
                    Orange  -> "D"
                    Yellow  -> "E"
                    Blue    -> "F"

instance Export RuleGroup where
  toJS rg =
    JS.object [ "rules" .= [ if Just (ruleName r) == actName
                               then toJS actR
                               else toJS r
                           | r <- ruleGroupRules rg ]
              , "activated" .= isJust actName
              ]
    where actName = (ruleName . activeOriginal) <$> actR
          actR    = ruleGroupActiveRule rg

instance Export ActiveRule where
  toJS ActiveRule { activeOriginal = Rule { .. }, .. } =
    JS.object $ ruleFields Rule { ruleInputs   = activeNeed, .. }
              ++ [ "have"      .= bagToList activeHave
                 , "fired"     .= activeFired
                 , "willreset" .= activeReset
                 ]

instance Export Rule where
  toJS = JS.object . ruleFields

ruleFields :: Rule -> [(Text,JS.Value)]
ruleFields Rule { .. } =
  [ "name"       .= ruleName
  , "inputs"     .= ruleInputs
  , "produce"    .= ruleProduces
  ]

instance Export RuleYield where
  toJS y =
    case y of
      Immediate as ->
        JS.object [ jsTag "immediate", "outputs" .= as ]
      LongTerm a ->
        JS.object [ jsTag "long_term", "outputs" .= a ]



instance Export (Bag Input) where
  toJS = toJS . bagToList

instance Export Input where
  toJS i =
    case i of
      Recall    -> JS.object [ "input"    .= JS.text "recall" ]
      Use m     -> JS.object [ "input"    .= JS.text "use"
                             , "material" .= m ]
      Discard m -> JS.object [ "input"    .= JS.text "discard"
                             , "material" .= m ]

instance Export AdjEffect where
  toJS a =
    case a of
      LooseGem      -> JS.text "looseGem"
      GainAction x  -> toJS x


instance Export ImmediateAction where
  toJS ImmediateAction { .. } =
    JS.object [ "actions"  .= bagToList playerActions
              , "adjacent" .= bagToList adjacentActions
              ]

instance Export LongTermAction where
  toJS la =
    case la of
      WhenProduce a b -> JS.object [ "long_term" .= JS.text "produce"
                                   , "action"    .= a
                                   , "upgrade"   .= b
                                   ]
      AtStart b       -> JS.object [ "long_term" .= JS.text "atStart"
                                   , "actions"   .= bagToList b
                                   ]

instance Export Upgrade where
  toJS u =
    case u of
      ConvertTo a -> JS.object [ "upgrade" .= JS.text "convert"
                               , "action"  .= a
                               ]
      Generate bs -> JS.object [ "upgrade" .= JS.text "generate"
                               , "actions" .= bagToList bs
                               ]



