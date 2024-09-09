{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Writer

data PauseT m r = RunT (m (PauseT m r)) | DoneT r
  -- deriving Show

-- instance (Show m, Show r) => Show PauseT where
--   show (DoneT r) = "DoneT (" ++ show r ++ ")"
--   show (RunT m) = "RunT (" ++ show m ++ ")"

instance (Functor m) => Functor (PauseT m) where
  fmap func (DoneT r) = DoneT (func r)
  fmap func (RunT m) = RunT $ fmap (fmap func) m

instance (Applicative m) => Applicative (PauseT m) where
  pure = DoneT
  DoneT f <*> DoneT a = DoneT (f a)
  DoneT f <*> RunT m = RunT $ fmap (\pause -> DoneT f <*> pause) m
  RunT m1 <*> pause = RunT $ fmap (\p -> p <*> pause) m1

-- show implement these
instance (Monad m) => Monad (PauseT m) where
  -- return :: Monad m => a -> PauseT m a
  return = pure
  -- (>>=) :: Monad m => PauseT m a -> (a -> PauseT m b) -> PauseT m b
  DoneT r >>= f = f r
  RunT m >>= f = RunT $ m >>= \pause -> return (pause >>= f)
  -- RunT m >>= f = RunT $ liftM (>>= f) m

instance MonadTrans PauseT where
  -- lift :: Monad m => m a -> PauseT m a
  lift m = RunT $ fmap DoneT m

instance MonadWriter w m => MonadWriter w (PauseT m) where
  tell = lift . tell

pause :: Monad m => PauseT m ()
pause = DoneT ()


-- bonus exercise, implement joinP
-- double bonus: without relying on PauseT's monad instance
-- triple bonus: explain in English what joinP *means* for the Pause monad
joinP :: Monad m => PauseT m (PauseT m a) -> PauseT m a
joinP (RunT m) = undefined

example1 :: PauseT (Writer String) ()
example1 = do
  tell "Step 1"
  tell "Step 2"
  tell "Step 3"

example2 :: PauseT IO ()
example2 = do
  lift $ putStrLn "Step 1"
  lift $ putStrLn "Step 2"
  lift $ putStrLn "Step 3"

fullRunT :: Monad m => PauseT m r -> m r
fullRunT (DoneT r) = return r
fullRunT (RunT m) = m >>= fullRunT

runNT :: (Monad m) => Int -> PauseT m r -> m (PauseT m r)
runNT 0 p = return p
runNT _ d@DoneT{} = return d
runNT n (RunT m) = m >>= runNT (n - 1)

-- main = do
--   fail "your turn"
--   fail "Fill in this main method with your own experiments"

-- show ...and see if it compiles.
main = putStrLn "it compiles"
