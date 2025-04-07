# → dualizer ←

[![Packaging status](https://repology.org/badge/tiny-repos/haskell:dualizer.svg)](https://repology.org/project/haskell:dualizer/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:dualizer.svg)](https://repology.org/project/haskell:dualizer/versions)
[![Join the chat at https://gitter.im/dualizer/Lobby](https://badges.gitter.im/dualizer/Lobby.svg)](https://gitter.im/dualizer/Lobby)

**Delete half (minus ε) of your Haskell code!**

## usage

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

### The Template Haskell You Need to Know

This library is written using Template Haskell, and while it tries to minimize the familiarity needed to use it (and accepting suggestions/PRs for reducing it further), some still leaks through. Here’s what you need to know.

When naming an _existing_ type, prefix with `''` (e.g., `''Either`), and for an _existing_ value, prefix with `'` (e.g., `'fmap`). Names of things to be created are plain `String`s.

To allow code to be reified into an AST that Template Haskell can work with, it uses a special “quasiquotation” syntax, opening with `[d|` and closing with `|]` that looks like `[d|your :: Code -> Here|]` when used. There are variants of this that use something other than `d` in the opening, but we don’t need them in this library.

### Defining Duals

There are three approaches here to defining duals, and they are listed in order of preference.

1. define them simultaneously
2. define the dual of an existing thing
3. label two existing things as duals of each other

Simultaneous definition is the only way to automatically define duals of expressions. If you define the dual of an existing value, you will only get its type, and you will still need to provide the expression.

You can, however, still label existing values as duals of each other.

#### Defining Duals Simultaneously

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

#### Defining the Dual of an Existing Thing

If one side of the construct already exists, then you can assign the duals like

```haskell
makeDualType 'cata "ana"
makeDualType ''Algebra "Coalgebra"
```

```haskell
makeDualClass ''Applicative "Coapplicative" [('pure, "extract")]
makeDualClass ''Monad "Comonad" [('(>>=) , "=>>")]
```

#### Labeling Existing Duals

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

## versioning

This project largely follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP), but is more strict in some ways.

The version always has four components, `A.B.C.D`. The first three correspond to those required by PVP, while the fourth matches the “patch” component from [Semantic Versioning](https://semver.org/).

Here is a breakdown of some of the constraints:

### sensitivity to additions to the API

PVP recommends that clients follow [these import guidelines](https://wiki.haskell.org/Import_modules_properly) in order that they may be considered insensitive to additions to the API. However, this isn’t sufficient. We expect clients to follow these additional recommendations for API insensitivity

If you don’t follow these recommendations (in addition to the ones made by PVP), you should ensure your dependencies don’t allow a range of `C` values. That is, your dependencies should look like

```cabal
yaya >=1.2.3 && <1.2.4
```

rather than

```cabal
yaya >=1.2.3 && <1.3
```

#### use package-qualified imports everywhere

If your imports are [package-qualified](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html?highlight=packageimports#extension-PackageImports), then a dependency adding new modules can’t cause a conflict with modules you already import.

#### avoid orphans

Because of the transitivity of instances, orphans make you sensitive to your dependencies’ instances. If you have an orphan instance, you are sensitive to the APIs of the packages that define the class and the types of the instance.

One way to minimize this sensitivity is to have a separate package (or packages) dedicated to any orphans you have. Those packages can be sensitive to their dependencies’ APIs, while the primary package remains insensitive, relying on the tighter ranges of the orphan packages to constrain the solver.

### transitively breaking changes (increments `A`)

#### removing a type class instance

Type class instances are imported transitively, and thus changing them can impact packages that only have your package as a transitive dependency.

#### widening a dependency range with new major versions

This is a consequence of instances being transitively imported. A new major version of a dependency can remove instances, and that can break downstream clients that unwittingly depended on those instances.

A library _may_ declare that it always bumps the `A` component when it removes an instance (as this policy dictates). In that case, only `A` widenings need to induce `A` bumps. `B` widenings can be `D` bumps like other widenings, Alternatively, one may compare the APIs when widening a dependency range, and if no instances have been removed, make it a `D` bump.

### breaking changes (increments `B`)

#### restricting an existing dependency’s version range in any way

Consumers have to contend not only with our version bounds, but also with those of other libraries. It’s possible that some dependency overlapped in a very narrow way, and even just restricting a particular patch version of a dependency could make it impossible to find a dependency solution.

#### restricting the license in any way

Making a license more restrictive may prevent clients from being able to continue using the package.

#### adding a dependency

A new dependency may make it impossible to find a solution in the face of other packages dependency ranges.

### non-breaking changes (increments `C`)

#### adding a module

This is also what PVP recommends. However, unlike in PVP, this is because we recommend that package-qualified imports be used on all imports.

### other changes (increments `D`)

#### widening a dependency range for non-major versions

This is fairly uncommon, in the face of `^>=`-style ranges, but it can happen in a few situations.

#### deprecation

**NB**: This case is _weaker_ than PVP, which indicates that packages should bump their major version when adding `deprecation` pragmas.

We disagree with this because packages shouldn’t be _publishing_ with `-Werror`. The intent of deprecation is to indicate that some API _will_ change. To make that signal a major change itself defeats the purpose. You want people to start seeing that warning as soon as possible. The major change occurs when you actually remove the old API.

Yes, in development, `-Werror` is often (and should be) used. However, that just helps developers be aware of deprecations more immediately. They can always add `-Wwarn=deprecation` in some scope if they need to avoid updating it for the time being.

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20dualizer).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## comparisons

Other projects similar to this one, and how they differ.
