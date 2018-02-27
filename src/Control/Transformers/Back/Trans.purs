module Control.Transformers.Back.Trans (runBackT, BackT(..), FailBack(..)) where

import Prelude

import Control.Alternative (class Alt, class Alternative)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class Plus)
import Control.MonadZero (class MonadZero)

data FailBack a = BackPoint a | NoBack a | GoBack
newtype BackT m a = BackT (m(FailBack a))

runBackT :: forall a b. BackT a b -> a (FailBack b)
runBackT (BackT x) = x

mapFailBack :: ∀ a b. (a → b) → FailBack a → FailBack b
mapFailBack f fb = case fb of
                        BackPoint a → BackPoint (f a)
                        NoBack a → NoBack (f a)
                        GoBack → GoBack

instance functorFailback :: Functor FailBack where
  map = mapFailBack

instance showFailback :: (Show a) => Show (FailBack a) where
  show (NoBack a) = "NoBack " <> (show a)
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
  lift = BackT <<< (map NoBack)

instance monadStateBackT :: MonadState s m => MonadState s (BackT m) where
  state = lift <<< state

instance monadRecBackT :: MonadRec m ⇒ MonadRec (BackT m) where
  tailRecM :: ∀ a b m. MonadRec m ⇒ (a → BackT m (Step a b)) → a → BackT m b
  tailRecM f = BackT <<< tailRecM \a → (runBackT $ f a) >>= \m' → pure $ case m' of
                                                                           NoBack (Loop a') → Loop a'
                                                                           NoBack (Done b) → Done (NoBack b)
                                                                           BackPoint (Loop a') → Loop a'
                                                                           BackPoint (Done b) → Done (BackPoint b)
                                                                           GoBack → Done GoBack

instance monadEffBackT :: MonadEff eff m ⇒ MonadEff eff (BackT m) where
  liftEff = lift <<< liftEff

instance altBackT :: Monad m ⇒ Alt (BackT m) where
  alt (BackT m1) (BackT m2) = BackT do
                                m ← m1
                                case m of
                                     GoBack → pure GoBack
                                     BackPoint a → do
                                        m' ← m2
                                        pure $ case m' of
                                          GoBack → GoBack
                                          _ → BackPoint a
                                     NoBack a → do
                                        m'← m2
                                        pure $ case m' of
                                          GoBack → GoBack
                                          BackPoint a' → BackPoint a'
                                          _ → NoBack a

instance plusBackT :: Monad m ⇒ Plus (BackT m) where
  empty = BackT $ pure GoBack

instance alternativeBackT :: Monad m ⇒ Alternative (BackT m)

instance monadZeroBackT :: Monad m ⇒ MonadZero (BackT m)
