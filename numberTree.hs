import Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numberTreeWithState tree) 1

test :: State Integer (Tree Integer)
test = state $ \counter -> ((Leaf counter), counter + 1)

numberTreeWithState :: Tree () -> State Integer (Tree Integer)
numberTreeWithState tree = case tree of
  (Leaf _)     -> state $ \counter -> ((Leaf counter), counter + 1)
  (Fork l _ r) -> ((state $ \counter -> (runState (numberTreeWithState l) counter)) >>= (\leftTree -> state $ \counter' -> ((Fork leftTree counter'), counter' + 1)))
    <*> (state $ \counter'' -> (runState (numberTreeWithState r) counter''))

-- (s -> (a, s') >>= (\a -> (\s' -> (b, s''))))

tree = (Fork (Leaf ()) () (Leaf ()))

-- GHCi> numberTree (Leaf ())
-- Leaf 1
-- GHCi> numberTree tree
-- Fork (Leaf 1) 2 (Leaf 3)