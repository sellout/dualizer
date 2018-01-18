{-# LANGUAGE TemplateHaskell #-}

-- | Dual mappings for types in 'base'.
module Categorical.Dual.Base where

import Prelude hiding (id)
import Categorical.Dual
import Categorical.Dual.Prelude
import Control.Arrow
import Control.Category

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
