
module MM.Control.Monad.S.ME(S) where
import Control.Monad
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype S e s m a = S {unM :: forall o. (e -> m o) -> (a -> s -> m o) -> s -> m o}
instance Functor (S e s m) where
  fmap f (S g) = S (\j k -> g j (k . f))
instance Monad (S e s m) where
  return a = S (\j k -> k a)
  S g >>= f = S (\j k -> g j (\a -> unM (f a) j k))
instance Applicative (S e s m) where
  pure = return
  (<*>) = ap
instance (Monad m) => RunM (S e s m) a (s -> m (Either e (a, s))) where
  runM (S g) = g (return . Left) (\a s-> return (Right (a, s)))
instance StateM (S e s m) s where
  get = S (\_ k s -> k s s)
  gets f = S (\_ k s -> k (f s) s)
  set s = S (\_ k _ -> k () s)
  modify f = S (\_ k -> k () . f)
instance (Monad m) => LiftM (S e s m) m where
  lift m = S (\_ k s -> flip k s =<< m)
instance (Monad m) => AbortM (S e s m) e where
  abort e = S (\j _ _ -> j e)

