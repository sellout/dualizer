{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Dual.Example where

import Data.Char
import Dual.TH

importPreludeDuals

-- TODO: this is much uglier than type families
testF :: $(dualType =<< [t|Int -> Char|])
testF = Data.Char.ord

testT :: $(dualType =<< [t|Either Int Char|])
testT = (7, 'a')

testQ :: $(dualType =<< [t|forall a b. Either (a -> Int) Char -> (Bool, Either Char (Int -> b))|])
testQ = undefined :: Either Bool (Char, b -> Int) -> (Int -> a, Char)

-- Not sure if this will need different ones for Class, Data, Value, etc.
labelSelfDual ''Functor
-- labelSelfDual 'fmap

labelDualData ''Either ''(,)

-- These are done as separate dual mappings (rather than something like `labelDualClass`) to ease a lot of the issues with not-quite dual constructions.
-- labelDual ''Monad ''Comonad -- `fail` has no dual, so it’ll fail to convert if
                            -- that method is hit, but not otherwise.
-- labelDual 'pure   'extract -- these operations exist in different classes
-- labelSemiDual 'return 'extract -- only maps one way, hopefully using some other
                               -- mapping for the other direction, good for
                               -- aliases, especially overconstrained ones.
-- labelDual '(>>=) '(=>>)
-- labelDual 'join 'duplicate -- the latter is a class method, but the former is a
                           -- function

makeDualClass ''Applicative "Coapplicative" [('pure, "extract")]

makeDualClass ''Monad "Comonad" [('(>>=) , "=>>")]

-- labelSemiDual 'return 'extract

-- makeDualValue 'join 'duplicate
-- makeDualValue '(=<<) '(<<=) -- aka, extend
-- makeDualValue '(>=>) '(=>=)
-- makeDualValue '(<=<) '(=<=)

exportDuals "importExampleDuals"
