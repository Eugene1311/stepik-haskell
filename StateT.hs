{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Writer

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)



evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT stateT x = fmap fst (runStateT stateT x)

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT stateT x = fmap snd (runStateT stateT x)

readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT readerT = StateT $ \s -> fmap (\a -> (a, s)) (runReaderT readerT s)

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \ s -> return (x, s)

  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'

instance MonadTrans (StateT s) where
  -- lift :: Monad m => m a ->  StateT s m a
  lift monad = StateT (\s -> fmap (\a -> (a, s)) monad)

instance MonadFail m => MonadFail (StateT s m) where
  fail str = (lift . fail) str

-- GHCi> sl2 = StateT $ \st -> [(st,st),(st+1,st-1)]
-- GHCi> runStateT (do {6 <- sl2; return ()}) 5
-- [((),4)]
-- GHCi> sm = StateT $ \st -> Just (st+1,st-1)
-- GHCi> runStateT (do {42 <- sm; return ()}) 5
-- Nothing

instance MonadWriter w m => MonadWriter w (StateT s m) where
  writer = lift . writer
  tell = lift . tell
  -- listen stateT = StateT $ \s -> lift $ listen (runStateT stateT s)
  -- pass stateT = StateT $ \s -> lift $ pass (runStateT stateT s)
