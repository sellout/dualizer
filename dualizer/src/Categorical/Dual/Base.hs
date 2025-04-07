{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
-- FIXME: remove this
{-# OPTIONS_GHC -Wwarn=missing-import-lists #-}

-- | Dual mappings for types in 'base'.
module Categorical.Dual.Base
  ( baseDuals,
  )
where

import Categorical.Dual
import Categorical.Dual.Prelude
import safe Control.Arrow
import safe Control.Category
import safe Prelude hiding (id)

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
