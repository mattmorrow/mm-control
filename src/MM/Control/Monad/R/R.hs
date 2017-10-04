
module MM.Control.Monad.R.R(R) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype R r a = R {unR :: forall o. (a -> o) -> r -> o}
instance Functor (R r) where
  fmap f (R g) = R (\k -> g (k . f))
instance Monad (R r) where
  return a = R (\k _ -> k a)
  R g >>= f = R (\k r -> g (\a -> unR (f a) k r) r)
instance Applicative (R r) where
  pure = return
  (<*>) = ap
instance RunM (R r) a (r -> a) where
  runM (R g) = g id
instance ReaderM (R r) r where
  ask = R (\k r -> k r)
  asks f = R (\k -> (k . f))
  local f (R g) = R (\k -> g k . f)

