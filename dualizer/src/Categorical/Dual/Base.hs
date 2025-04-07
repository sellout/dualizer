{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}

-- | Dual mappings for types in 'base'.
module Categorical.Dual.Base
  ( baseDuals,
  )
where

import Categorical.Dual
  ( exportDuals,
    importDuals,
    labelDual,
    labelSelfDual,
    labelSemiDual,
  )
import Categorical.Dual.Prelude (preludeDuals)
import safe Control.Arrow
  ( Arrow,
    arr,
    returnA,
    (&&&),
    (***),
    (+++),
    (<<<),
    (>>>),
    (|||),
  )
import safe Control.Category (Category)
import safe qualified Control.Category as Category
import safe qualified Prelude as Prelude

importDuals preludeDuals

labelSelfDual ''Arrow
labelSelfDual 'arr
labelDual '(***) '(+++)
labelDual '(&&&) '(|||)
labelSelfDual 'returnA

labelSelfDual ''Category
labelSelfDual 'Prelude.id
labelSelfDual 'Category.id
labelSemiDual '(Prelude..) '(>>>)
labelSemiDual '(Category..) '(>>>)
labelDual '(<<<) '(>>>)

-- | Duals for 'base'.
exportDuals "baseDuals"
