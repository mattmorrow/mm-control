
module MM.Control.Monad.Class (
   RunM(..)
  ,LiftM(..)
  ,STM(..)
  ,IOM(..)
  ,StateM(..)
  ,ReaderM(..)
  ,WriterM(..)
  ,ContM(..)
  ,ExceptionM(..)
  ,AbortM(..)
  ,UniqM(..)
  ,Uniq2M(..)
) where
import Control.Monad.ST(ST)

-- | .
class (Monad m) => RunM m a b | m a -> b where
  runM :: m a -> b

class (Monad m, Monad n) => LiftM m n | m -> n where
  lift :: n a -> m a

class (Monad m) => STM m where
  stM :: ST s a -> m a

class (Monad m) => IOM m where
  ioM :: IO a -> m a

class (Monad m) => StateM m s | m -> s where
  get :: m s
  set :: s -> m ()
  gets :: (s -> a) -> m a
  modify :: (s -> s) -> m ()
  gets f = do x <- get; let {!a = f x}; return a
  modify f = do x <- get; let {!y = f x}; set y

class (Monad m) => ReaderM m r | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  asks :: (r -> a) -> m a
  asks f = do x <- ask; let {!a = f x}; return a

class (Monad m) => WriterM m w | m -> w where
  put :: w -> m ()
  collect :: m a -> m (a, w)

class (Monad m) => ContM m where
  callCC :: ((a -> m b) -> m a) -> m a

class Monad m => ExceptionM m e | m -> e where
  raise :: e -> m a
  try :: m a -> m (Either e a)

class Monad m => AbortM m e | m -> e where
  abort :: e -> m a

class (Monad m) => UniqM m where
  newUniqM :: m Int
  getUniqM :: m Int
  setUniqM :: Int -> m ()
  swapUniqM :: Int -> m Int
  newUniqM = do
    i <- getUniqM
    let !j = i + 1
    setUniqM j
    return i
  swapUniqM i = do
    j <- getUniqM
    setUniqM i
    return j

class (Monad m) => Uniq2M m where
  newUniq1M :: m Int
  getUniq1M :: m Int
  setUniq1M :: Int -> m ()
  swapUniq1M :: Int -> m Int
  newUniq2M :: m Int
  getUniq2M :: m Int
  setUniq2M :: Int -> m ()
  swapUniq2M :: Int -> m Int
  newUniq1M = do
    i <- getUniq1M
    let !j = i + 1
    setUniq1M j
    return i
  swapUniq1M i = do
    j <- getUniq1M
    setUniq1M i
    return j
  newUniq2M = do
    i <- getUniq2M
    let !j = i + 1
    setUniq2M j
    return i
  swapUniq2M i = do
    j <- getUniq2M
    setUniq2M i
    return j

