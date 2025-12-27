module MyRWT where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Char

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt env = runWriterT $ runReaderT rwt env

myAsk :: Monad m => MyRWT m [String]
myAsk = ask

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell str = ReaderT $ \_ -> tell str

myLift :: Monad m => m a -> MyRWT m a
myLift monad = ReaderT (\_ -> WriterT $ (\x -> (x, "")) <$> monad)

myWithReader :: Monad m => ([String] -> [String]) -> MyRWT m a -> MyRWT m a
myWithReader = withReaderT

-- logFirstAndRetSecond :: MyRWT IO String
-- logFirstAndRetSecond = do
--   el1 <- myAsks head
--   myLift $ putStrLn $ "First is " ++ show el1
--   el2 <- myAsks (map toUpper . head . tail)
--   myLift $ putStrLn $ "Second is " ++ show el2
--   myTell el1
--   return el2

-- GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
-- First is "abc"
-- Second is "DEFG"
-- ("DEFG","abc")

-- logFirstAndRetSecond :: MyRWT Maybe String
-- logFirstAndRetSecond = do
--   xs <- myAsk
--   case xs of
--     (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
--     _ -> myLift Nothing
