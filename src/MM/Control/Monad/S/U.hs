
module MM.Control.Monad.S.U(S) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype S s a = S {unS :: forall o. (a -> Int -> s -> o) -> Int -> s -> o}
instance Functor (S s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance Applicative (S s) where
  pure = return
  (<*>) = ap
instance RunM (S s) a (Int -> s -> (a, (Int, s))) where
  runM (S g) (i) = g (\a i s-> (a,(i,s))) i
instance StateM (S s) s where
  get = S (\k i s-> k s i s)
  gets f = S (\k i s-> k (f s) i s)
  set s = S (\k i _-> k () i s)
  modify f = S (\k i s-> k () i (f s))
instance UniqM (S s) where
  newUniqM = S (\k i-> k (i) (i + 1))
  getUniqM = S (\k i-> k (i) i)
  setUniqM (i) = S (\k _ -> k () i)
  swapUniqM (j) = S (\k i-> k (i) j)

