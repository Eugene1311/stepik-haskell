import Control.Applicative
import Control.Monad.Identity

data Logged a = Logged String a deriving (Eq,Show)

-- why not LoggedT ??
newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Functor f => Functor (LoggT f) where
  -- fmap :: (a -> b) -> LoggT f a -> LoggT f b
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
