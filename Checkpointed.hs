type Checkpointed a = ((a -> r) -> r) -- it's a monad, it's a function that accepts function as a parameter

-- runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
-- runCheckpointed = undefined

-- addTens :: Int -> Checkpointed Int
-- addTens x1 = \checkpoint -> do
--   checkpoint x1
--   let x2 = x1 + 10
--   checkpoint x2     {- x2 = x1 + 10 -}
--   let x3 = x2 + 10
--   checkpoint x3     {- x3 = x1 + 20 -}
--   let x4 = x3 + 10
--   return x4         {- x4 = x1 + 30 -}

-- GHCi> runCheckpointed (< 100) $ addTens 1
-- 31
-- GHCi> runCheckpointed  (< 30) $ addTens 1
-- 21
-- GHCi> runCheckpointed  (< 20) $ addTens 1
-- 11
-- GHCi> runCheckpointed  (< 10) $ addTens 1
-- 1
