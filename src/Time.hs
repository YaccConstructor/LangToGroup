-- |Module `Time` include useful functions for working calculations which must
--  be done after a certain period of time.
module Time (
    TimeAccuracy (MS, S, M, H),
    MonadTimeout,
    timeout,
    timeout',
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Timeout as T
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Lazy as S

data TimeAccuracy =
    MS
  | S
  | M
  | H

toMS :: RealFrac i => i -> TimeAccuracy -> i
toMS t MS = t
toMS t S  = t * 1000.0
toMS t M  = t * 60000.0
toMS t H  = t * 3600000.0

timeout' :: RealFrac i => i -> TimeAccuracy -> IO a -> IO (Maybe a)
timeout' t ta = T.timeout (round $ toMS t ta * 1000.0)

class Monad m => MonadTimeout m where
    timeout :: (RealFrac i, NFData a) => i -> TimeAccuracy -> m a -> m a

instance MonadTimeout IO where
    timeout t ta a = do
        maybeRes <- timeout' t ta a
        case maybeRes of
            Nothing  -> fail "Computation is so long"
            Just res -> return res

instance MonadTimeout Maybe where
    timeout t ta a = do
        val <- a
        unsafePerformIO $ timeout' t ta $ evaluate $ force val

instance MonadTimeout m => MonadTimeout (R.ReaderT r m) where
    timeout t ta (R.ReaderT a) = R.ReaderT $ timeout t ta . a

instance MonadTimeout m => MonadTimeout (S.StateT s m) where
    timeout t ta (S.StateT a) = S.StateT $ \s -> do
        (val, s') <- a s
        val' <- timeout t ta $ return val
        return (val', s')
