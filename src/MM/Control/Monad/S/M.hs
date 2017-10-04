
module MM.Control.Monad.S.M(S) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype S m s a = S {unS :: forall o. (a -> s -> m o) -> s -> m o}
instance Functor (S m s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S m s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance (MonadFix m) => MonadFix (S m s) where
  mfix f = S (\k s ->
    uncurry k =<< mfix (\ ~(a,_) ->
      unS (f a) (\a s -> return (a,s)) s))
instance Applicative (S m s) where
  pure = return
  (<*>) = ap
instance (Monad m) => RunM (S m s) a (s -> m (a, s)) where
  runM (S g) = g ((return .) . (,))
instance StateM (S m s) s where
  get = S (\k s -> k s s)
  gets f = S (\k s -> k (f s) s)
  set s = S (\k _ -> k () s)
  modify f = S (\k -> k () . f)
instance (Monad m) => LiftM (S m s) m where
  lift m = S (\k s -> flip k s =<< m)

