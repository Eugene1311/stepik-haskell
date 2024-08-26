{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import Control.Monad.Identity
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader

data Logged a = Logged String a deriving (Eq,Show)

-- why not LoggedT ??
newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Functor f => Functor (LoggT f) where
  -- fmap :: (a -> b) -> LoggT m a -> LoggT m b
  fmap func loggT = LoggT (fmap updater (runLoggT loggT))
    where updater (Logged str x) = Logged str (func x)

instance Applicative app => Applicative (LoggT app) where
  pure x = LoggT $ pure (Logged "" x)
  -- (<*>) :: Applicative app => LoggT app (a -> b) -> LoggT app a -> LoggT app b
  loggT1 <*> loggT2  = LoggT $
    liftA2 updater (runLoggT loggT1) (runLoggT loggT2)
      where updater ~(Logged a f) ~(Logged b x) = Logged (a `mappend` b) (f x)

instance Monad m => Monad (LoggT m) where
  return = pure
  -- (>>=) :: Monad m => LoggT m a -> (a -> LoggT m b) -> LoggT m b
  loggT >>= k = LoggT $ do
    (Logged str1 x) <- (runLoggT loggT)
    (Logged str2 y) <- runLoggT (k x)
    return (Logged (str1 `mappend` str2) y)

instance MonadFail m => MonadFail (LoggT m) where
  fail msg = LoggT (fail msg)

logTst :: LoggT Identity Integer
logTst = do 
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z
  
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

-- runIdentity (runLoggT logTst)
-- Logged "AAABBB" 42
-- runLoggT $ failTst [5,5]
-- [Logged "A" 42,Logged "A" 42]
-- runLoggT $ failTst [5,6]
-- [Logged "A" 42]
-- runLoggT $ failTst [7,6]
-- []

write2log :: Monad m => String -> LoggT m ()
write2log str = do
  LoggT $ return $ Logged str ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT 

logTst' :: Logg Integer   
logTst' = do 
  write2log "AAA"
  write2log "BBB"
  return 42

-- GHCi> runLogg logTst'
-- Logged "AAABBB" 42

instance MonadTrans LoggT where
  lift m = LoggT $ fmap (Logged "") m

logSt :: LoggT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

-- GHCi> runState (runLoggT logSt) 2
-- (Logged "30" 300,42)

instance MonadState s m => MonadState s (LoggT m) where
  get   = lift get
  put   = lift . put
  -- state :: MonadState s m => (s -> (a, s)) -> LoggT m a - (m (Logged a))
  -- state = \func -> LoggT $ fmap (Logged "") (state func)
  state = lift . state

logSt' :: LoggT (State Integer) Integer
logSt' = do
  modify (+1)                   -- no lift!
  a <- get                      -- no lift!
  write2log $ show $ a * 10
  put 42                        -- no lift!
  return $ a * 100

-- GHCi> runState (runLoggT logSt') 2
-- (Logged "30" 300,42)

instance MonadReader r m => MonadReader r (LoggT m) where
  ask = lift ask
  -- local :: MonadReader r m => (r -> r) -> m a -> m a
  local = mapLoggT . local
  reader = lift . reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f loggT = LoggT $ f (runLoggT loggT)

-- logRdr :: LoggT (Reader [(Int,String)]) ()
-- logRdr = do 
--   Just x <- asks $ lookup 2                      -- no lift!
--   write2log x
--   Just y <- lift $ local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
--   write2log y

-- GHCi> runReader (runLoggT logRdr) [(1,"John"),(2,"Jane")]
-- Logged "JaneJim" ()

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log = write2log
  logg (Logged str a) = return a

logSt'' :: LoggT (State Integer) Integer
logSt'' = do
  x <- logg $ Logged "BEGIN " 1
  modify (+x)
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

-- GHCi> runState (runLoggT logSt'') 2
-- (Logged "BEGIN 30 END" 300,42)

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg  = lift . logg

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer      
rdrStLog = do
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  modify (+ (x+y))
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

-- GHCi> runLogg $ runStateT (runReaderT rdrStLog 4) 2
-- Logged "BEGIN 70 END" (700,42)
