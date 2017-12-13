# → dualizer ←

**Delete half (minus ε) of your Haskell code!**

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

See `Dual.Example` for a bit more.
