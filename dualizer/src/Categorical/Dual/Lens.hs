{-# LANGUAGE TemplateHaskell #-}

-- | Dual mappings for types in 'lens'.
module Categorical.Dual.Lens where

import Categorical.Dual
import Categorical.Dual.Prelude
import Control.Lens

importDuals preludeDuals

labelDual ''Lens ''Prism
labelDual 'alongside 'without

labelSelfDual ''Iso

labelSelfDual ''Equality

-- | Duals for 'lens'.
exportDuals "lensDuals"
