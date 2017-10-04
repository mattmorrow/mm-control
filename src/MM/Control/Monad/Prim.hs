
module MM.Control.Monad.Prim (
   castST
  ,module Control.Monad.ST.Strict
) where

import Control.Monad
import Control.Monad.ST.Strict
import Unsafe.Coerce(unsafeCoerce)
import GHC.IO(unsafeSTToIO,unsafeIOToST)
import MM.Control.Monad.Class

instance RunM (ST s) a a where
  runM m = runST (unsafeCoerce m)

castST :: ST s a -> ST t a
castST m = unsafeCoerce m

instance STM IO where
  stM = unsafeSTToIO

instance IOM (ST s) where
  ioM = unsafeIOToST

