{-# LANGUAGE TemplateHaskell #-}

module Dual.Base where

import Prelude hiding (id)
import Control.Arrow
import Control.Category
import Dual.Prelude
import Dual.TH

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

exportDuals "baseDuals"
