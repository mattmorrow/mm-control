
module MM.Control.Monad.S.ST(S) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import Control.Monad.ST(ST)
import MM.Control.Monad.Class
import MM.Control.Monad.Prim(castST)

newtype S z s a = S {unS :: forall o. (a -> s -> ST z o) -> s -> ST z o}
instance Functor (S z s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S z s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance MonadFix (S z s) where
  mfix f = S (\k s ->
    uncurry k =<< mfix (\ ~(a,_) ->
      unS (f a) (\a s -> return (a,s)) s))
instance Applicative (S z s) where
  pure = return
  (<*>) = ap
instance RunM (S z s) a (s -> ST z (a, s)) where
  runM (S g) = g (\a s -> return (a,s))
instance StateM (S z s) s where
  get = S (\k s -> k s s)
  gets f = S (\k s -> k (f s) s)
  set s = S (\k _ -> k () s)
  modify f = S (\k -> k () . f)
instance STM (S z s) where
  stM m = S (\k s-> do a <- castST m; k a s)

