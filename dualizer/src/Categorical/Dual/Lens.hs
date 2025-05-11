{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}

-- | Dual mappings for types in 'lens'.
module Categorical.Dual.Lens
  ( lensDuals,
  )
where

import Categorical.Dual (exportDuals, importDuals, labelDual, labelSelfDual)
import Categorical.Dual.Prelude (preludeDuals)
import safe Control.Lens (Equality, Iso, Lens, Prism, alongside, without)

importDuals preludeDuals

labelDual ''Lens ''Prism
labelDual 'alongside 'without

labelSelfDual ''Iso

labelSelfDual ''Equality

-- | Duals for 'lens'.
exportDuals "lensDuals"
