{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Unsafe #-}

{- ORMOLU_DISABLE -}
{- because it can’t handle CPP within a declaration -}
-- | This should be tests, but if you look for the source of this module,
--   you’ll see how to use the package.
module Categorical.Dual.Example
  ( Coapplicative (..),
    Comonad (..),
    Distributive (..),
    consume,
    Algebra,
    Coalgebra,
    GAlgebra,
    GCoalgebra,
    ElgotAlgebra,
    ElgotCoalgebra,
    NewEither (..),
    NewTuple (..),
    NewEither' (..),
    NewTuple' (..),
    TestA,
    DualA,
    TestB,
    DualB,
#if MIN_VERSION_template_haskell(2, 16, 0)
#else
    TestC,
    DualC,
#endif
    (>^>),
    (<^<),
    Mu (..),
    Nu (..),
    Fix (..),
    cata,
    ana,
    exampleDuals,
    testF,
    testT,
    testV,
    testV',
    testQ,
  )
where
{- ORMOLU_ENABLE -}

import Categorical.Dual
  ( dualType,
    exportDuals,
    importDuals,
    labelDual,
    labelSelfDual,
    labelSemiDual,
    makeDualClass,
    makeDualDec,
  )
import Categorical.Dual.Base (baseDuals)
import Categorical.Dual.Lens (lensDuals)
import safe Control.Applicative (Applicative, pure)
import safe Control.Arrow ((>>>))
import safe Control.Category ((.))
import safe Control.Monad (Monad, (=<<), (>>=))
import safe Data.Bool (Bool)
import safe Data.Char (Char, ord)
import safe Data.Either (Either (Right))
import safe Data.Foldable (Foldable)
import safe Data.Function (($))
import safe Data.Functor (Functor, fmap)
import safe Data.Int (Int)
import safe Data.Traversable (Traversable)
import safe qualified Data.Traversable as T
import safe Data.Void (Void)
import safe Prelude (undefined)

importDuals baseDuals
importDuals lensDuals

-- TODO: this is much uglier than type families
testF :: $(dualType =<< [t|Int -> Char|])
testF = ord

testT :: $(dualType =<< [t|Either Int Char|])
testT = (7, 'a')

testV :: $(dualType =<< [t|Either () Char|])
testV = undefined :: (Void, Char)

testV' :: $(dualType =<< [t|((), Char)|])
testV' = Right 'a' :: Either Void Char

testQ :: $(dualType =<< [t|forall a b. Either (a -> Int) Char -> (Bool, Either Char (Int -> b))|])
testQ = undefined :: Either Bool (Char, b -> Int) -> (Int -> a, Char)

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

-- | This should get mapped to the newly created class … right?
makeDualClass ''Applicative "Coapplicative" [('pure, "extract")]

-- | This should get mapped to the newly created class … right?
makeDualClass ''Monad "Comonad" [('(>>=), "=>>")]

-- FIXME: These semi-duals can be dangerous. It’s fine for overconstrained
--        mappings (like `mapM` -> `traverse`), but in cases like `Foldable` and
--       `Applicative`, you can’t reasonably round-trip. I.e., you can’t auto-
--        dualize `Traversable` from `Distributive`, because the constraint will
--        be too weak.
labelSemiDual ''Foldable ''Functor

makeDualClass
  ''Traversable
  "Distributive"
  [ ('T.traverse, "cotraverse"),
    ('T.sequenceA, "distribute")
  ]

-- TODO: Doesn’t really belong here, but is the dual to `collect`.
consume :: (Traversable g, Applicative f) => (g b -> a) -> g (f b) -> f a
consume f = fmap f . T.sequenceA

-- labelSemiDual 'return 'extract

-- makeDualValue 'join 'duplicate
-- makeDualValue '(=<<) '(<<=) -- aka, extend
-- makeDualValue '(>=>) '(=>=)
-- makeDualValue '(<=<) '(=<=)

-- | Sometimes the doc is mapped to the original.
makeDualDec [d|type Algebra f a = f a -> a|] "Coalgebra"

-- | Other times, to the dual.
makeDualDec [d|type GAlgebra w f a = f (w a) -> a|] "GCoalgebra"

-- | I’m not sure why one or the other happens.
makeDualDec [d|type ElgotAlgebra w f a = w (f a) -> a|] "ElgotCoalgebra"

makeDualDec [d|newtype NewEither a b = NewEither (Either a b)|] "NewTuple"
makeDualDec [d|data NewEither' a b = NewEither' (Either a b)|] "NewTuple'"

-- FIXME: doesn’t terminate
-- makeDualDec [d|data Mu f = Mu (forall a. Algebra f a -> a)|] "NotNu"

-- | I wonder if
makeDualDec [d|data family TestA a |] "DualA"

-- | This always
makeDualDec [d|type family TestB a |] "DualB"

-- FIXME: Complains “‘TestC_0’ is not in scope at a reify”
#if MIN_VERSION_template_haskell(2, 16, 0)
#else
-- | Happens.
makeDualDec
  [d| type family TestC a where
        TestC (Either b _c) = b
        TestC Int = Char |]
  "DualC"
#endif

-- | These docs are going to end up on `<^<`, which is not what I’d expect.
makeDualDec
  [d|
    (>^>) :: (a -> b) -> (b -> c) -> a -> c
    (>^>) = (>>>)
    |]
  "<^<"

-- withDual [d| { infix 3 >^> } |]

labelSelfDual '($)

data Mu f = Mu (forall a. Algebra f a -> a)

data Nu f where Nu :: Coalgebra f a -> a -> Nu f

labelDual ''Mu ''Nu

data Fix f = Fix {unfix :: f (Fix f)}

labelSelfDual ''Fix -- not really
labelDual 'Fix 'unfix

-- | Interestingly, the documentation for a dualized function definition is
--   added to the dual, not the explicitly-defined name. I don’t know why this
--   behaves differently than the other cases.
makeDualDec
  [d|
    cata :: (Functor f) => (f a -> a) -> Fix f -> a
    cata f = f . fmap (cata f) . unfix
    |]
  "ana"

-- | Duals for this module.
exportDuals "exampleDuals"
