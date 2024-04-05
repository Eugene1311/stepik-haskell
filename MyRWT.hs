import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Char

type MyRWT m = ReaderT [String] (WriterT String m)
-- ReaderT r m a
-- ReaderT [String] m a

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt env = runWriterT $ runReaderT rwt env

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

-- tell :: Monad m => w -> WriterT w m ()
myTell :: Monad m => String -> MyRWT m ()
myTell str = ReaderT $ \_ -> tell str

-- ReaderT :: (r -> m a) -> ReaderT r m a
-- ghci> :t WriterT
-- WriterT :: m (a, w) -> WriterT w m a
myLift :: Monad m => m a -> MyRWT m a
myLift monad = ReaderT (\_ -> WriterT $ (\x -> (x, "")) <$> monad)

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

-- GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
-- First is "abc"
-- Second is "DEFG"
-- ("DEFG","abc")
