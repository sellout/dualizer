{-# LANGUAGE TemplateHaskell #-}

module Dual.Lens where

import Control.Lens
import Dual.Prelude
import Dual.TH

importDuals preludeDuals

labelDual ''Lens ''Prism
labelDual 'alongside 'without

labelSelfDual ''Iso

labelSelfDual ''Equality

exportDuals "lensDuals"
