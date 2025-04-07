{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
-- FIXME: remove this
{-# OPTIONS_GHC -Wwarn=implicit-prelude
    -Wwarn=missing-export-lists
    -Wwarn=missing-import-lists #-}

-- | Dual mappings for types in 'lens'.
module Categorical.Dual.Lens where

import Categorical.Dual
import Categorical.Dual.Prelude
import safe Control.Lens

importDuals preludeDuals

labelDual ''Lens ''Prism
labelDual 'alongside 'without

labelSelfDual ''Iso

labelSelfDual ''Equality

-- | Duals for 'lens'.
exportDuals "lensDuals"
