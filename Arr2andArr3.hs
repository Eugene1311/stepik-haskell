import Control.Monad.Trans.Class

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap g (Arr2 f) = Arr2 (\x -> \y -> g(f x y))

instance Functor (Arr3 e1 e2 e3) where
  fmap g (Arr3 f) = Arr3 (\x -> \y -> \z -> g(f x y z))

-- func :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- func f g = \x -> \y -> f(g x y)

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T (\x y -> return (f x y))

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T (\x y z -> return (f x y z))

instance (Functor m) => Functor (Arr2T e1 e2 m) where
  fmap g (Arr2T f) = Arr2T (\x -> \y -> fmap g (f x y))

instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where
  fmap g (Arr3T f) = Arr3T (\x -> \y -> \z -> fmap g (f x y z))

instance (Applicative m) => Applicative (Arr2T e1 e2 m) where
  pure a = Arr2T (\_ -> \_ -> pure a)
  (<*>) (Arr2T f1) (Arr2T f2) = Arr2T (\x -> \y -> (f1 x y) <*> (f2 x y))

instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
  pure a = Arr3T (\_ -> \_ -> \_ -> pure a)
  (<*>) (Arr3T f1) (Arr3T f2) = Arr3T (\x -> \y -> \z -> (f1 x y z) <*> (f2 x y z))

-- f :: Arr2T(\e1 -> \e2 -> m a), g :: \a -> Arr2(\e1 -> \e2 -> m b)
instance (Monad m) => Monad (Arr2T e1 e2 m) where
  f >>= g = Arr2T $ \r1 -> \r2 -> do
    val <- getArr2T f r1 r2
    getArr2T (g val) r1 r2

instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
  f >>= g = Arr3T $ \r1 -> \r2 -> \r3 -> do
    val <- getArr3T f r1 r2 r3
    getArr3T (g val) r1 r2 r3

instance MonadTrans (Arr2T e1 e2) where
  -- lift :: Monad m => m a ->  Arr2T e1 e2 m
  lift m = Arr2T (\_ -> \_ -> m)

instance MonadTrans (Arr3T e1 e2 e3) where
  -- lift :: Monad m => m a ->  Arr3T e1 e2 e3 m
  lift m = Arr3T (\_ -> \_ -> \_ -> m)

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T (\e1 -> \e2 -> return (f e1 e2))

instance (MonadFail m) => MonadFail (Arr2T e1 e2 m) where
  fail str = Arr2T (\_ -> \_ -> (fail str))

instance (MonadFail m) => MonadFail (Arr3T e1 e2 e3 m) where
  fail str = lift (fail str)
