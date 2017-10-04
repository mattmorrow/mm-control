
module MM.Control.Monad.I.I(I) where
import Control.Monad
import Control.Monad.Fix(MonadFix(..))
import Control.Applicative(Applicative(..))
import Data.Function(fix)
import MM.Control.Monad.Class

newtype I a = I {unI::a}
runI :: I a -> a
runI = unI
instance Functor I where
  fmap f (I a) = I (f a)
instance Monad I where
  return = I
  I a >>= k = k a
instance MonadFix I where
  mfix f = fix (f . unI)
instance Applicative I where
  pure = return
  (<*>) = ap
instance RunM I a a where
  runM = unI

