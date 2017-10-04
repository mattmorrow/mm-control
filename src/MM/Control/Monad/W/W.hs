
module MM.Control.Monad.W.W(W) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import Data.Monoid(Monoid(..))
import MM.Control.Monad.Class

newtype W w a = W {unW :: forall o. (a -> w -> o) -> o}
instance Functor (W w) where
  fmap f (W g) = W (\k -> g (k . f))
instance (Monoid w) => Monad (W w) where
  return a = W (\k -> k a mempty)
  W g >>= f = W (\k -> g (\a w -> unW (f a) (\a w' -> k a (w`mappend`w'))))
instance (Monoid w) => Applicative (W w) where
  pure = return
  (<*>) = ap
instance (Monoid w) => RunM (W w) a (a, w) where
  runM (W g) = g (,)
instance (Monoid w) => WriterM (W w) w where
  put w = W (\k -> k () w)
  collect (W g) = W (\k-> g (\a w-> k (a,w) w))

