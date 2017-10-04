
module MM.Control.Monad.S.S(S) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype S s a = S {unS :: forall o. (a -> s -> o) -> s -> o}
instance Functor (S s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance MonadFix (S s) where
  mfix f = S (\k s -> let (a,s') = unS (f a) (,) s in k a s')
instance Applicative (S s) where
  pure = return
  (<*>) = ap
instance RunM (S s) a (s -> (a, s)) where
  runM (S g) = g (,)
instance StateM (S s) s where
  get = S (\k s -> k s s)
  gets f = S (\k s -> k (f s) s)
  set s = S (\k _ -> k () s)
  modify f = S (\k -> k () . f)

