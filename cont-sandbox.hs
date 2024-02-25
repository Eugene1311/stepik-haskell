import Control.Monad.Trans.Cont
import Control.Monad       (when)

test :: Cont [r] Int
test = cont $ \c -> (c 2) ++ (c 3)

foo :: Int -> Cont r String
foo x = callCC $ \k1 -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k1 "over twenty"
  cont $ \k2 -> k2 (show $ y - 4) -- return (show $ y - 4)

-- instance Monad []  where
--     {-# INLINE (>>=) #-}
--     xs >>= f             = [y | x <- xs, y <- f x]
