module Control.Transformers.Back.Trans (runBackT, BackT(..), FailBack(..)) where

import Prelude
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)

data FailBack a = BackPoint a | NoBack a | GoBack
newtype BackT m a = BackT (m(FailBack a))

runBackT :: forall a b. BackT a b -> a (FailBack b)
runBackT (BackT x) = x

mapFailBack :: ∀ a b. (a → b) → FailBack a → FailBack b
mapFailBack f fb = case fb of
                        BackPoint a → BackPoint (f a)
                        NoBack a → NoBack (f a)
                        GoBack → GoBack

applyFailback :: ∀ a b. FailBack (a → b) → FailBack a → FailBack b
applyFailback GoBack _ = GoBack
applyFailback _ GoBack = GoBack
applyFailback (NoBack f) (NoBack x) = NoBack (f x)
applyFailback (NoBack f) (BackPoint x) = NoBack (f x)
applyFailback (BackPoint f) (NoBack x) = NoBack ( f x)
applyFailback (BackPoint f) (BackPoint x) = NoBack (f x)

instance functorFailback :: Functor FailBack where
  map = mapFailBack

instance applicativeFailback :: Applicative FailBack where
  pure = NoBack

instance applyFailback' :: Apply FailBack where
  apply = applyFailback

instance showFailback :: (Show a) => Show (FailBack a) where
  show (NoBack a) = "NoBack" <> (show a)
  show (BackPoint a) = "BackPoint " <> (show a)
  show (GoBack) = "GoBack"

instance functorFailbackT :: (Functor f) => Functor (BackT f) where
  map f m = BackT $ map (mapFailBack f) (runBackT m)

instance applyFailbackT :: (Monad m) => Apply (BackT m) where
  apply = ap


instance applicativeFailbackT :: (Monad m) => Applicative (BackT m) where
  pure = BackT <<< pure <<< NoBack

instance bindFailbackT :: (Monad m) => Bind (BackT m) where
  bind x f = BackT $ loop
                where loop = do
                        v <- runBackT x
                        case v of
                          NoBack y -> runBackT (f y)
                          BackPoint y -> do
                            z <- runBackT (f y)
                            case z of
                              GoBack -> loop
                              other -> pure other
                          GoBack -> pure GoBack

instance monadFailbackT :: (Monad m) => Monad (BackT m)

instance monadTransBackT :: MonadTrans BackT where
  lift m = BackT $ NoBack <$> m

instance monadStateBackT :: MonadState s m => MonadState s (BackT m) where
  state f = lift (state f)
