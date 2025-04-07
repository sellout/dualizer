{-# LANGUAGE TemplateHaskell #-}
-- FIXME: remove this
{-# OPTIONS_GHC -Wwarn=implicit-prelude
    -Wwarn=missing-export-lists
    -Wwarn=missing-exported-signatures
    -Wwarn=missing-import-lists
    -Wwarn=missing-safe-haskell-mode
    -Wwarn=missing-signatures #-}

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
