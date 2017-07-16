module State where

import Types

data State = State
  { builtLocs :: [ ActiveLoc ]
  , dealLocs  :: [ Location ]

  , handLocs  :: [ Location ]
  , handConns :: [ Connection ]

  , resources :: [ Resource ]
  }


data ActiveLoc = ActiveLoc
  { actBasic  :: Location   -- ^ The static loca
  , actAvail  :: Int
    -- ^ How many times can this be activated.
    -- For actions and open production
  , actStored :: [(Int,Resource)]
    -- ^ Some loctaions allow storing of items across turns

  }


hasSecretStock :: State -> Bool
hasSecretStock = any matches . builtLocs
  where matches l = case locationText (actBasic l) of
                      SecretStock -> True
                      _           -> False

decreaseDefenceBy :: State -> Int
decreaseDefenceBy = sum . map getMod . builtLocs
  where getMod d = case locationText (actBasic d) of
                     DecreaseDefence n -> n
                     _ -> 0





