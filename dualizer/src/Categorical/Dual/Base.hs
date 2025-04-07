{-# LANGUAGE TemplateHaskell #-}
-- FIXME: remove this
{-# OPTIONS_GHC -Wwarn=implicit-prelude
    -Wwarn=missing-export-lists
    -Wwarn=missing-import-lists
    -Wwarn=missing-safe-haskell-mode #-}

-- | Dual mappings for types in 'base'.
module Categorical.Dual.Base where

import Categorical.Dual
import Categorical.Dual.Prelude
import Control.Arrow
import Control.Category
import Prelude hiding (id)

importDuals preludeDuals

labelSelfDual ''Arrow
labelSelfDual 'arr
labelDual '(***) '(+++)
labelDual '(&&&) '(|||)
labelSelfDual 'returnA

labelSelfDual ''Category
labelSelfDual 'id
labelSemiDual '(Prelude..) '(>>>)
labelSemiDual '(Control.Category..) '(>>>)
labelDual '(<<<) '(>>>)

-- | Duals for 'base'.
exportDuals "baseDuals"
