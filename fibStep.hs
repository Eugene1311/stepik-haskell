import Control.Monad.State

fibStep :: State (Integer, Integer) ()
fibStep = do
  (a, b) <- get
  put (b, a + b)
  return ()

-- GHCi> execState fibStep (0,1)
-- (1,1)
-- GHCi> execState fibStep (1,1)
-- (1,2)
-- GHCi> execState fibStep (1,2)
-- (2,3)

execStateN :: Int -> State s a -> s -> s
execStateN 0 state s = s
execStateN n state s = execStateN (n - 1) state (execState state s)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)
