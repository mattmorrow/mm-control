
module MM.Control.Monad.C.C(C) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import MM.Control.Monad.Class

newtype C r a = C {unC :: forall o. (a -> r) -> (r -> o) -> o}
instance Functor (C r) where
  fmap f (C g) = C (\k z -> g (k . f) z)
instance Monad (C r) where
  return a = C (\k z -> z (k a))
  C g >>= f = C (\k z -> g (\a -> unC (f a) k id) z)
  -- C g >>= f = C (\k z -> g (\a -> unC (f a) k z) id
instance Applicative (C r) where
  pure = return
  (<*>) = ap
instance RunM (C r) a ((a -> r) -> r) where
  runM (C g) = flip g id
instance ContM (C r) where
  --callCC :: ((a -> (forall b. C r b)) -> C r a) -> C r a
  callCC f = C (\k z -> unC (f (\a -> C (\_ h -> (h . k) a))) k z)

idC :: C a a -> a
idC = flip runM id
mapC :: (r -> r) -> C r a -> C r a
mapC f (C g) = C (\k z -> g (f . k) z)
-- mapC f (C g) = C (\k z -> g k (z . f))
withC :: ((b -> r) -> (a -> r)) -> C r a -> C r b
withC f (C g) = C (\k z -> g (f k) z)
shift  :: ((a -> (forall s. C s r)) -> C r r) -> C r a
shift f = C (\k z -> unC (f (\a -> C (\e h -> (h . e . k) a))) id z)
reset :: C a a -> C r a
reset m = C (\k z -> (z . k) (runM m id))

