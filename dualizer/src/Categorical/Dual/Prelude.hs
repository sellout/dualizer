{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}

-- | Dual mappings for types in the 'Prelude'.
module Categorical.Dual.Prelude
  ( preludeDuals,
  )
where

import Categorical.Dual
  ( emptyDuals,
    exportDuals,
    importDuals,
    labelDual,
    labelSelfDual,
  )
import safe Data.Bool (Bool, not, (&&), (||))
import safe Data.Char (Char)
import safe Data.Either (Either (Left, Right))
import safe Data.Eq (Eq, (/=), (==))
import safe Data.Function (const, id)
import safe Data.Functor (Functor, fmap, (<$), (<$>))
import safe Data.Int (Int)
import safe Data.List (map)
import safe Data.Ord (Ord, Ordering, max, min, (<), (<=), (>), (>=))
import safe Data.Ratio (Rational)
import safe Data.String (String)
import safe Data.Tuple (fst, snd)
import safe Data.Word (Word)
import safe System.IO
  ( IO,
    getChar,
    getLine,
    putChar,
    putStrLn,
    readFile,
    writeFile,
  )
import safe Text.Read (Read, read)
import safe Text.Show (Show, show)
import safe Prelude
  ( Bounded,
    Double,
    Enum,
    Float,
    Floating,
    Fractional,
    Integer,
    Integral,
    Num,
    Real,
    RealFrac,
    maxBound,
    minBound,
  )

importDuals emptyDuals

labelSelfDual ''Bool
labelDual '(&&) '(||)
labelSelfDual 'not

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

-- | Duals for the 'Prelude'.
exportDuals "preludeDuals"
