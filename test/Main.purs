module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM, random, randomInt)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..), runBackT)

backpointRandomInt :: forall eff. BackT (Eff ( random :: RANDOM | eff)) Int
backpointRandomInt = BackT $ BackPoint <$> (randomInt 1 10)

main :: Eff (console :: CONSOLE, random :: RANDOM) (FailBack Unit)
main = runBackT $ do
  _ ← liftEff $ log "Starting Run"
  a ← backpointRandomInt
  _ ← liftEff $ log "Generated a number"
  _ ← if (a >= 9) then pure unit else BackT (pure GoBack)
  b ← backpointRandomInt
  _ ← liftEff $ log "Generated another number"
  _ ← if (b >= 9) then pure unit else BackT (pure GoBack)
  liftEff $ logShow (a + b)

testRec :: Number → (Eff (random :: RANDOM, console :: CONSOLE)) (FailBack Number)
testRec r = runBackT $ tailRecM loop r
  where loop i | i > 0.999 = pure $ Done i
               | otherwise = (liftEff $ log "Generating a new number") *> (liftEff $ Loop <$> random)
