
module MM.Control.Monad.S.U2(S) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype S s a = S {unS :: forall o. (a -> Int -> Int -> s -> o) -> Int -> Int -> s -> o}
instance Functor (S s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance Applicative (S s) where
  pure = return
  (<*>) = ap
instance RunM (S s) a (Int -> Int -> s -> (a, (Int, Int, s))) where
  runM (S g) (i) (j) = g (\a i j s-> (a,(i,j,s))) i j
instance StateM (S s) s where
  get = S (\k i j s-> k s i j s)
  gets f = S (\k i j s-> k (f s) i j s)
  set s = S (\k i j _-> k () i j s)
  modify f = S (\k i j s-> k () i j (f s))
instance UniqM (S s) where
  newUniqM = newUniq1
  getUniqM = getUniq1
  setUniqM = setUniq1
  swapUniqM = swapUniq1
instance Uniq2M (S s) where
  newUniq1M = newUniq1
  getUniq1M = getUniq1
  setUniq1M = setUniq1
  swapUniq1M = swapUniq1
  newUniq2M = newUniq2
  getUniq2M = getUniq2
  setUniq2M = setUniq2
  swapUniq2M = swapUniq2
newUniq1 :: S s Int
getUniq1 :: S s Int
setUniq1 :: Int -> S s ()
swapUniq1 :: Int -> S s Int
newUniq1 = S (\k i j-> k (i) (i + 1) j)
getUniq1 = S (\k i j-> k (i) i j)
setUniq1 (i) = S (\k _ j -> k () i j)
swapUniq1 (i') = S (\k i j-> k (i) i' j)
newUniq2 :: S s Int
getUniq2 :: S s Int
setUniq2 :: Int -> S s ()
swapUniq2 :: Int -> S s Int
newUniq2 = S (\k i j-> k (j) i (j + 1))
getUniq2 = S (\k i j-> k (j) i j)
setUniq2 (j) = S (\k i _-> k () i j)
swapUniq2 (j') = S (\k i j-> k (j) i j')

