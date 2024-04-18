import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
  -> (Integer,Integer)  
  -> Integer 
  -> m (Either String a, Integer)
runRiiEsSiT readerT env st = runStateT (runExceptT (runReaderT readerT env)) st

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go stateT = ReaderT $ \bounds -> do
  x <- lift $ get
  n <- execStateT stateT x
  lift $ put n
  lift $ helper bounds n

-- go :: Integer -> Integer -> State Integer Integer -> EsSi ()
-- go lowerBound upperBound state = ExceptT $ do
--   x <- get
--   let n = execState state x
--   put n
--   return $ (helper lowerBound upperBound n)

helper :: (Integer, Integer) -> Integer -> Either String ()
helper (lowerBound, upperBound) n
  | n >= upperBound = (Left "Upper bound")
  | n <= lowerBound = (Left "Lower bound")
  | otherwise = (Right ())

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

-- GHCi> runRiiEsSiT (forever $ go tickCollatz') (1,200) 27
-- 82
-- 41
-- 124
-- 62
-- 31
-- 94
-- 47
-- 142
-- 71
-- 214
-- (Left "Upper bound",214)
