import Control.Monad.Trans.Except
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

instance Functor (FailCont r e) where
    fmap = liftM

instance Applicative (FailCont r e) where
    pure x = FailCont (\ok _ -> ok x)
    (<*>) = ap

instance Monad (FailCont r e) where
  -- ((a -> r) -> (e -> r) -> r) -> (a -> ((b -> r) -> (e -> r) -> r)) -> ((b -> r) -> (e -> r) -> r)
  fc >>= func = FailCont $ \ok notOk -> runFailCont fc (\a -> runFailCont (func a) ok notOk) notOk
  -- (\a -> func a ok notOk) :: a -> r


toFailCont :: Except e a -> FailCont r e a
toFailCont ex = FailCont $ \ok notOk -> case (runExcept ex) of
  Left e -> notOk e
  Right a -> ok a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont failCont = runFailCont failCont Right Left

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \ok notOk -> runFailCont (f (\a -> FailCont $ \ _ _ -> ok a)) ok notOk

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead str | null str = throwE EmptyInput
            | null result = throwE (NoParse str)
            | (not . null) $ snd (result !! 0) = throwE (NoParse str)
            | otherwise = return $ fst (result !! 0)
                where result = reads str

-- GHCi> evalFailCont $ addInts "15" "12"
-- Right 27
-- GHCi> runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show)
-- Oops: EmptyInput

-- chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
-- chainCPS s f = \k -> s (\a -> f a k)
-- s :: (a -> r), (\a -> f a k) :: (a -> r)), k :: (b -> r) -> r

-- instance Monad (Cont r) where
--     return x = cont ($ x)
--     s >>= f  = cont $ \c -> runCont s $ \x -> runCont (f x) c

-- callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
-- callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
-- f :: (a -> Cont r b) -> Cont r a
-- cont :: ((a -> r) -> r) -> Cont r a
-- h :: a -> r
-- \_ -> h a :: _ -> r
-- cont $ \_ -> h a :: Cont r _
-- \a -> cont $ \_ -> h a :: a -> Cont r _ || a -> Cont r a ??
-- 

-- callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
-- callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
-- callCFC f = FailCont $ \ok notOk -> runFailCont (f (\a -> FailCont $ \ _ _ -> ok a)) ok notOk
