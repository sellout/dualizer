# → dualizer ←

**Delete half (minus ε) of your Haskell code!**

[![Join the chat at https://gitter.im/dualizer/Lobby](https://badges.gitter.im/dualizer/Lobby.svg)](https://gitter.im/dualizer/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Dualizer allows you to eliminate the dual of all your code. Rather than implementing, say, `Comonad` directly, you can define it in terms of its dual – `Monad`:

```haskell
-- indicates that Functor is its own dual
labelSelfDual ''Functor

-- expands to:
--
--   class Functor f => Coapplicative f where
--     extract :: f a -> a -- the dual of pure
makeDualClass ''Applicative "Coapplicative" [('pure, "extract")]

-- expands to:
--
--   class Coapplicative m => Comonad m where
--     (=>>) :: m b -> (m b -> a) -> m a
makeDualClass ''Monad "Comonad" [('(>>=) , "=>>")]
```

See [`Categorical.Dual.Example`](src/Categorical/Dual/Example.hs) for a bit more.

## The Template Haskell You Need to Know

This library is written using Template Haskell, and while it tries to minimize the familiarity needed to use it (and accepting suggestions/PRs for reducing it further), some still leaks through. Here’s what you need to know.

When naming an _existing_ type, prefix with `''` (e.g., `''Either`), and for an _existing_ value, prefix with `'` (e.g., `'fmap`). Names of things to be created are plain `String`s.

To allow code to be reified into an AST that Template Haskell can work with, it uses a special “quasiquotation” syntax, opening with `[d|` and closing with `|]` that looks like `[d|your :: Code -> Here|]` when used. There are variants of this that use something other than `d` in the opening, but we don’t need them in this library.

## Defining Duals

There are three approaches here to defining duals, and they are listed in order of preference.

1. define them simultaneously
2. define the dual of an existing thing
3. label two existing things as duals of each other

Simultaneous definition is the only way to automatically define duals of expressions. If you define the dual of an existing value, you will only get its type, and you will still need to provide the expression.

You can, however, still label existing values as duals of each other.

## Defining Duals Simultaneously

```haskell
makeDualDec
  [d| cata :: Functor f => (f a -> a) -> Fix f -> a
      cata f = f . fmap (cata f) . unfix |]
  "ana"

makeDualDec [d|type Algebra f a = f a -> a|] "Coalgebra"
```

This form can also be nested, allowing the definition of duals for type classes, etc. (NB: This can’t actually work this way).
```haskell
makeDualDec [d|
  class Functor f => Applicative f where
    $$(makeDualDec [d|pure :: a -> f a|] "extract")
|] "Coapplicative"

makeDualDec [d|
  class Applicative f => Monad f where
    $$(makeDualDec [d|>>= :: f a -> (a -> f b) -> f b|] "=>>")
    fail :: f ()
|] "Comonad"
```

## Defining the Dual of an Existing Thing

If one side of the construct already exists, then you can assign the duals like

```haskell
makeDualType 'cata "ana"
makeDualType ''Algebra "Coalgebra"
```

```haskell
makeDualClass ''Applicative "Coapplicative" [('pure, "extract")]
makeDualClass ''Monad "Comonad" [('(>>=) , "=>>")]
```

## Labeling Existing Duals

Labeling is especially useful when things are duals of themselves.

```haskell
labelSelfDual ''Functor
labelSelfDual 'fmap -- not implied by the former because:

class Steppable t f | t -> f where
  project :: t -> f t
  embed :: f t -> t

labelSelfDual ''Steppable
labelDual 'project 'embed
```

Also, if there are things that are both equivalent to some other thing, you can label one as “semi-dual”, mapping in one direction but not the other.

```haskell
labelDual 'pure 'extract

-- `return` is overconstrained, so we let it dualize to `extract`, but `extract`
-- will be converted to `pure` on any return trip.
labelSemiDual 'return 'extract
```
