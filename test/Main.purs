module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..), runBackT)

backpointRandomInt :: forall eff. BackT (Eff ( random :: RANDOM | eff)) Int
backpointRandomInt = BackT $ BackPoint <$> (randomInt 1 10)

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
