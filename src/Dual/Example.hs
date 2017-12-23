{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Dual.Example where

import Data.Char
import Dual.Base
import Dual.Lens
import Dual.TH

importDuals baseDuals
importDuals lensDuals

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

labelDual ''Either ''(,)

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


-- FIXME: These semi-duals can be dangerous. It’s fine for overconstrained
--        mappings (like `mapM` -> `traverse`), but in cases like `Foldable` and
--       `Applicative`, you can’t reasonably round-trip. I.e., you can’t auto-
--        dualize `Traversable` from `Distributive`, because the constraint will
--        be too weak.
labelSemiDual ''Foldable ''Functor

makeDualClass ''Traversable "Distributive"
  [ ('traverse, "cotraverse")
  , ('sequenceA, "distribute")]

-- TODO: Doesn’t really belong here, but is the dual to `collect`.
consume :: (Traversable g, Applicative f) => (g b -> a) -> g (f b) -> f a
consume f = fmap f . sequenceA

-- labelSemiDual 'return 'extract

-- makeDualValue 'join 'duplicate
-- makeDualValue '(=<<) '(<<=) -- aka, extend
-- makeDualValue '(>=>) '(=>=)
-- makeDualValue '(<=<) '(=<=)

makeDualDec [d|type Algebra f a = f a -> a|]            "Coalgebra"
makeDualDec [d|type GAlgebra w f a = f (w a) -> a|]     "GCoalgebra"
makeDualDec [d|type ElgotAlgebra w f a = w (f a) -> a|] "ElgotCoalgebra"

labelSelfDual '($)

data Fix f = Fix { unfix :: f (Fix f) }
labelSelfDual ''Fix -- not really
labelDual 'Fix 'unfix

makeDualDec
  [d| cata :: Functor f => (f a -> a) -> Fix f -> a
      cata f = f . fmap (cata f) . unfix |]
  "ana"

exportDuals "exampleDuals"
