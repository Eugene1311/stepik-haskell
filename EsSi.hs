import Control.Monad.State
import Control.Monad.Trans.Except

import Debug.Trace

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi esSi x = runState (runExceptT esSi) x

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lowerBound upperBound state = ExceptT $ do
  x <- get
  let n = execState state x
  put n -- `debug` ("x: " ++ show x)
  return $ (helper lowerBound upperBound n) -- `debug` ("n: " ++ show n)
  -- if n > upperBound then return (Left "Upper bound")
  --   else if n < lowerBound then return (Left "Lower bound")
  --     else return (Right ())

debug = flip trace

helper :: Integer -> Integer -> Integer -> Either String ()
helper lowerBound upperBound n
  | n >= upperBound = (Left "Upper bound")
  | n <= lowerBound = (Left "Lower bound")
  | otherwise = (Right ())

-- GHCi> runEsSi (go 1 85 tickCollatz) 27
-- (Right (),82)
-- GHCi> runEsSi (go 1 80 tickCollatz) 27
-- (Left "Upper bound",82)
-- GHCi> runEsSi (forever $ go 1 1000 tickCollatz) 27
-- (Left "Upper bound",1186)
-- GHCi> runEsSi (forever $ go 1 10000 tickCollatz) 27
-- (Left "Lower bound",1)
