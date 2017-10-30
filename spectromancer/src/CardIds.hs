{-# Language OverloadedStrings, TemplateHaskell #-}
module CardIds where

import Cards(card_name_decls)

$card_name_decls


