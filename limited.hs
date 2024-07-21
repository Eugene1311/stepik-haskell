import Control.Monad.Except
import Control.Monad.State
import Data.Foldable

-- run1 :: (MonadState s f, MonadError e f) => f [b] -> s -> (Either Int [a], s)
run1 x s = runState (runExceptT x) s
run2 x s = runExcept $ runStateT x s

limited :: (MonadState s f, MonadError e f, Num e, Enum e) => (s -> Bool) -> [State s a] -> f [b]
limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s

-- GHCi> runLimited1 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- (Left 2,3)
-- GHCi> runLimited2 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- Left 2

-- GHCi> runLimited1 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- (Right [(),(),(),()],4)
-- GHCi> runLimited2 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- Right ([(),(),(),()],4)