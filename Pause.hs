data Pause m
  = Run (m (Pause m))
  | Done

pauseExample1 :: Pause IO
pauseExample1 = Run $ do
  putStrLn "Let's begin"
  putStrLn "Step 1"
  return $ Run $ do
    putStrLn "Step 2"
    return $ Run $ do
      putStrLn "Step 3"
      putStrLn "Yay, we're done!"
      return Done

runN :: Monad m => Int -> Pause m -> m (Pause m)
runN _ Done = return Done
runN 0 pause =return pause
runN n (Run m) = m >>= runN (n - 1)

fullRun :: Monad m => Pause m -> m ()
fullRun Done = return ()
fullRun (Run m) = m >>= fullRun

-- show Check the result
main = do
  rest <- runN 2 pauseExample1
  putStrLn "=== should print through step 2 ==="
  Done <- runN 1 rest
  -- remember, IO Foo is just a recipe for Foo, not a Foo itself
  -- so we can run that recipe again
  fullRun rest
  fullRun pauseExample1
