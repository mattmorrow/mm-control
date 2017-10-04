
module MM.Control.Monad.S.IO(S) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype S s a = S {unS :: forall o. (a -> s -> IO o) -> s -> IO o}
instance Functor (S s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance MonadFix (S s) where
  mfix f = S (\k s ->
    uncurry k =<< mfix (\ ~(a,_) ->
      unS (f a) (\a s -> return (a,s)) s))
instance Applicative (S s) where
  pure = return
  (<*>) = ap
instance RunM (S s) a (s -> IO (a, s)) where
  runM (S g) = g (\a -> return . (,) a)
instance StateM (S s) s where
  get = S (\k s -> k s s)
  gets f = S (\k s -> k (f s) s)
  set s = S (\k _ -> k () s)
  modify f = S (\k -> k () . f)
instance IOM (S s) where
  ioM m = S (\k s -> flip k s =<< m)

