# purescript-backtrack

A monad transformer for implementing backtracking in continuation based program.

This library is purescript equivalent of Haskell library [`failback-monad`](http://haskell-web.blogspot.com/2012/03/failback-monad.html)

## Installation

```
bower install purescript-backtrack
```

## Example

```
main :: forall e. Eff (console::CONSOLE,random:: RANDOM|e) (FailBack Unit)
main = runBackT $ do
  _ ← BackT $ pure <$> log "Starting Run"
  a ← backpointRandomInt
  _ ← BackT $ pure <$> log "Generated a number"
  _ ← if (a >= 9) then pure unit else BackT (pure GoBack)
  b ← backpointRandomInt
  _ ← BackT $ pure <$> log "Generated another number"
  _ ← if (b >= 9) then pure unit else BackT (pure GoBack)
  BackT $ pure <$> logShow (a + b)

backpointRandomInt :: forall eff. BackT (Eff ( random :: RANDOM | eff)) Int
backpointRandomInt = BackT $ BackPoint <$> (randomInt 1 10)

```
