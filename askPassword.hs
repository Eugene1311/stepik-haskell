import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)

newtype PwdError = PwdError String
  deriving Show

instance Semigroup PwdError where
  PwdError a <> PwdError b = PwdError (a ++ b)

instance Monoid PwdError where
  mempty = PwdError ""

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  validate s

validate :: String -> PwdErrorIOMonad String
validate s | length s < 8 = printAndThrow "Incorrect input: password is too short!"
           | not $ any isNumber s = printAndThrow "Incorrect input: password must contain some digits!"
           | not $ any isPunctuation s = printAndThrow "Incorrect input: password must contain some punctuation!"
           | otherwise = return s

printAndThrow :: String -> PwdErrorIOMonad String
printAndThrow s = do
  liftIO $ putStrLn s
  throwE $ PwdError s

askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s

