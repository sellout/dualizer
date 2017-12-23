{-# LANGUAGE TemplateHaskell #-}

module Dual.Prelude where

import Dual.TH

importDuals initialDuals

labelSelfDual ''Bool
labelDual '(&&) '(||)
labelSelfDual 'not

labelSelfDual ''Maybe

labelDual ''Either ''(,)

labelSelfDual ''Ordering

labelSelfDual ''Char

labelSelfDual ''String

labelDual 'fst 'Left
labelDual 'snd 'Right

labelSelfDual ''Eq
labelSelfDual '(==)
labelSelfDual '(/=)

labelSelfDual ''Ord
labelDual '(<) '(>=)
labelDual '(<=) '(>)
labelDual 'max 'min

labelSelfDual ''Enum

labelSelfDual ''Bounded
labelDual 'minBound 'maxBound

labelSelfDual ''Int

labelSelfDual ''Integer

labelSelfDual ''Float

labelSelfDual ''Double

labelSelfDual ''Rational

labelSelfDual ''Word

labelSelfDual ''Num

labelSelfDual ''Real

labelSelfDual ''Integral

labelSelfDual ''Fractional

labelSelfDual ''Floating

labelSelfDual ''RealFrac

-- Monoid has a dual in the hask package (Comonoid)

labelSelfDual ''Functor
labelSelfDual 'fmap
labelSelfDual '(<$)
labelSelfDual '(<$>)

-- Monad has a dual in the comonads package (Comonad)

-- Traversable has a dual in the distributive package (Distributive)

labelSelfDual 'id
labelSelfDual 'const
-- labelSelfDual 'undefined

labelSelfDual 'map

labelDual ''Show ''Read
labelDual 'show 'read

labelSelfDual ''IO
labelDual 'putChar 'getChar
labelDual 'putStrLn 'getLine

labelDual 'readFile 'writeFile

exportDuals "preludeDuals"
