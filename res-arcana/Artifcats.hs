{-# Language OverloadedStrings #-}
module Artifacts where

import Prelude hiding ((>))

import BasicTypes
import Language
import Component

artifact :: Component
artifact = blankComponent Artifact

artifacts :: [ Component ]
artifacts =
  [ artifact {
      name = "Fiery Whip",
      cost = [ Pay 2 Fire, Pay 2 Death ],
      powers =
        [ This > Exhaust > 3 `times` gain Fire > Rivals (gain Fire)

        , This > Exhaust
          > ChooseC > Also > IsComponent Artifact
                    > Also > This > Different
                    > Also > Destroy
          > GetCost > Nat 2 > Add > Repeat (ChooseR > isNot Gold > Gain)
        ]
    }

  , artifact {
      name = "Chalice of Fire",
      cost = [ Pay 1 Gold, Pay 1 Fire ],
      collect = [ 2 `times` gain Fire ],
      powers = [ This > Exhaust > spend Fire > ChooseC > Restore ]
    }

  , artifact {
      name = "Chalice of Life",
      cost = [ Pay 1 Gold, Pay 1 Life, Pay 1 Water ],
      collect = [ gain Water, gain Life ],
      powers = [ This > IsReady
                 > 2 `times` spend Water
                 > 2 `times` (Resource Water > This > GainOn)
                 > Resource Life > This > GainOn ]
      -- XXX: REACTION
    }

  , artifact {
      name = "Hypnotic Basin",
      cost = [ Pay 2 Water, Pay 1 Fire, Pay 1 Death ],
      collect = [ 2 `times` gain Water ],
      powers = [ This > Exhaust
               > ChooseRi > Resource Fire > GetCount
               > Repeat (gain Water) ]
    }

  , artifact {
      name = "Mermaid",
      cost = [ Pay 2 Life, Pay 2 Water ],
      isLifeForm = [ Creature ],
      collect = [ 2 `times` gain Water ],
      powers = [ This > Exhaust
               > ChooseR > isNot Fire > isNot Death
               > Also > Spend
               > ChooseC > GainOn ]
    }

  , artifact {
      name = "Prism",
      powers = [ This > Exhaust > ChooseR > Spend
               > 2 `times` (ChooseR > isNot Gold > Gain)

               , This > Exhaust
               > ChooseN > Also
               > ChooseR > WithPrev > Repeat (Also > Spend) > Done
               > ChooseR > isNot Gold > WithPrev > Repeat (Also > Gain) > Done
               ]

    }

  , artifact {
      name = "Cursed Skull",
      cost = [ Pay 2 Death ],
      powers = [ This > Exhaust
                 > Resource Life > Spend
                 > ChooseR > isNot Gold > isNot Life
                 > 3 `times` (Also > This > GainOn) > Done
               ]
    }

  , artifact {
      name    = "Sacrificial Dagger",
      cost    = [ Pay 1 Death, Pay 1 Gold ],
      powers  = [ This > Exhaust > Resource Life > Spend >
                  3 `times` (Resource Death > This > GainOn)

                , This > Also > IsReady > Destroy
                  > ChooseC > Also > Discard
                  > GetCost > Repeat (ChooseR > isNot Gold > Gain)
                ]
    }

  , artifact {
      name = "Magical Shard",
      powers = [ This > Exhaust > ChooseR > isNot Gold > Gain ]
    }

  , artifact {
      name = "Elvish Bow",
      cost = [ Pay 2 Fire, Pay 1 Life ],
      powers = [ This > Exhaust > Rivals (attack 1)
               , This > Exhaust > Draw
               ]
    }

  , artifact {
      name = "Hand of Glory",
      cost = [ Pay 1 Life, Pay 1 Death ],
      powers = [ This > Exhaust > 2 `times` gain Death > Rivals (gain Death) ]
    }

  , artifact {
      name = "Treant",
      isLifeForm = [ Creature ],
      cost = [ Pay 3 Life, Pay 2 Fire ],
      collect = [ 2 `times` gain Life ],
      powers = [ This > Exhaust > ChooseRi
                 > Resource Fire > GetCount > Repeat (gain Death) ]
    }

  , artifact {
      name = "Windup Man",
      cost = [ Pay 1 Fire, Pay 1 Life, Pay 1 Water, Pay 1 Gold ],
      collect = [ NoCollect
                  > Resources
                      (Also > This > Contains
                            > 2 `times` (Also > This > GainOn) > Done) ],
      powers = [ This > Exhaust > ChooseR > Also > Spend
                 > This > GainOn ]
    }

  , artifact {
      name = "Corrupt Altar",
      cost = [ PayAny 3, Pay 2 Death ],
      collect = [ gain Life > gain Death ],
      powers = [ This > IsReady
                 > 2 `times` spend Life
                 > 3 `times` (Resource Fire > This > GainOn)
               , This > Exhaust
                 > ChooseC > Also > IsComponent Artifact
                           > Also > Destroy
                 > GetCost > Nat 2 > Add
                 > Repeat (ChooseR > isNot Gold > Gain) ]
    }

  , artifact {
      name = "Horn of Plenty",
      cost = [ Pay 2 Gold ],
      powers = [ This > Exhaust > 3 `times` (ChooseR > isNot Gold > Gain)
               , This > Exhaust > gain Gold ]
    }

  , artifact {
      name = "Ring of Midas",
      cost = [ Pay 1 Life, Pay 1 Gold ],
      powers = [ This > IsReady
                 > 2 `times` spend Life
                 > Resource Gold > This > GainOn
               , This > Exhaust
                 > Resource Gold > This > GainOn ],
      points = FixedPoints 1
    }

  , artifact {
      name = "Dwarven Pickaxe",
      cost = [ Pay 1 Fire ],
      powers = [ This > Exhaust > spend Fire > gain Gold ]
    }

  , artifact {
      name = "Dragon Teeth",
      cost = [ Pay 1 Fire, Pay 1 Death ],
      powers = [ This > IsReady
                 > 2 `times` spend Fire
                 > 3 `times` (Resource Fire > This > GainOn)

               , This > Exhaust
                 > 3 `times` (Resource Fire > Spend)
                 > ChooseC > Also > IsLifeForm Dragon
                           > Also > Located InHand
                 > Also > GetCost
                 > Repeat (ChooseR > GainDiscount)
                 > Build ]
    }

  , artifact {
      name = "Vault",
      cost = [ Pay 1 Gold, PayAny 1 ],
      collect = [ NoCollect
                  > Resource Gold > This > Contains
                  > 2 `times` (ChooseR > isNot Gold > Gain) ],
      powers = [ This > Exhaust
                 > Resource Gold > This > GainOn ]
    }

  , artifact {
      name = "Athanor",
      cost = [ Pay 1 Gold, Pay 1 Fire ],
      powers = [ This > Exhaust
                 > spend Fire
                 > 2 `times` (Resource Fire > This > GainOn)
               , This > Exhaust
                 > 6 `times` (Resource Fire > This > SpendFrom)
                 > ChooseN
                 > Also > ChooseR > WithPrev
                 > Repeat (Also > Spend) > Done
                 > Repeat (gain Gold) ]
    }

  , artifact {
      name = "Hawk",
      cost = [ Pay 1 Life, Pay 1 Water ],
      isLifeForm = [ Creature ],
      collect = [ gain Water ],
      powers = [ This > Exhaust > Nat 3 > Reorder
               , This > Exhaust > 2 `times` spend Water > Draw ]
    }

  , artifact {
      name = "Celecstial Horse",
      isLifeForm = [ Creature ],
      cost = [ Pay 2 Water, Pay 1 Fire ],
      collect = [ 2 `times` (ChooseR > isNot Gold > isNot Death > Gain) ]
    }

  , artifact {
      name = "Crypt",
      cost = [ PayAny 3, Pay 2 Death ],
      powers = [ This > Exhaust > 2 `times` gain Death
               , This > Exhaust > spend Death
                 > ChooseC > Also > Located InDiscard
                           > 2 `times` (ChooseR > isNot Gold > GainDiscount)
                           > Build ]
    }

  , artifact {
      name = "Flaming Pit",
      cost = [ Pay 2 Fire ],
      collect = [ gain Fire ],
      powers = [ This > Exhaust > spend Life > gain Fire > gain Death ]
    }

  , artifact {
      name = "Philosopher's Stone",
      cost = [ Pay 2 Fire, Pay 2 Life, Pay 2 Water, Pay 2 Death ],
      powers = [ This > Exhaust
               > 2 `times` (ChooseR > Spend)
               > ChooseN > Also
               > ChooseR > WithPrev > Repeat (Also > Spend) > Done
               > Repeat (gain Gold) ],
      points = FixedPoints 1
    }

    , artifact {
        name = "Tree of Life",
        cost = [ PayAny 2, Pay 1 Life ],
        powers = [ This > Exhaust
                   > 3 `times` gain Life
                   > Rivals (gain Life)
                   -- XXX: REACT
                 ]
      }

    , artifact {
        name = "Dragon Egg",
        cost = [ Pay 1 Gold ],
        powers = [ This > Also > IsReady > Destroy
                 > ChooseC > Also > IsLifeForm Dragon
                 > Also > Located InHand
                 > 4 `times` (ChooseR > GainDiscount) > Build
                 ],
        points = FixedPoints 1
      }

    , artifact {
        name = "Nightingale",
        cost = [ Pay 1 Life, Pay 1 Water ],
        isLifeForm = [ Creature ],
        points = FixedPoints 1
      }

    , artifact {
        name = "Jeweled Statuette",
        cost = [ Pay 2 Death, Pay 1 Gold ],
        powers = [ This > Exhaust
                   > 3 `times` gain Death
                   > Rivals (gain Death)

                 , This > Also > IsReady > Destroy
                   > 2 `times` gain Gold
                   > gain Fire
                 ],
        points = FixedPoints 1
      }

    , artifact {
        name = "Fountain of Youth",
        cost = [ Pay 1 Water, Pay 1 Death ],
        collect = [ gain Life ],
        powers = [ This > IsReady
                   > 2 `times` spend Death
                   > 2 `times` (Resource Water > This > GainOn)
                   > Resource Life > This > GainOn
                 ]
      }

    , artifact {
        name = "Guard Dog",
        cost = [ Pay 1 Fire ],
        isLifeForm = [ Creature ],
        powers = [ {- not ready! -}
                   spend Fire > This > Restore
                  -- XXX: REACTION!!
                 ]
      }

    , artifact {
        name = "Dragon Bridle",
        cost = [ Pay 1 Fire, Pay 1 Life, Pay 1 Water, Pay 1 Death ]
        -- XXX: ABILITY, REACTION
      }


  ]


dog1 :: Steps -- Ability
dog1 = Resource Fire > Spend > This > Restore


