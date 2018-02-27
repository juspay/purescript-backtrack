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

testRec :: Eff (random :: RANDOM, console :: CONSOLE) Unit
testRec = void <$> runBackT $ tailRecM loop {r : 0.0, c : 1}
  where loop i | i.r > 0.9999999 = (liftEff $ log $ "Didn't stackoverflow for " <> show i.c <> " iters") *> (pure $ Done i)
               | otherwise = (liftEff $ log "Generating a new number") *> (liftEff $ (\r → Loop {r : r, c : i.c + 1}) <$> random)
