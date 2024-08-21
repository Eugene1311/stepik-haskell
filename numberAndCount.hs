import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Monoid

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving Show

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go (Leaf x) = do
      nodeCounter <- get
      put (nodeCounter + 1)
      lift $ tell 1
      return (Leaf nodeCounter)
    go (Fork left x right) = do
      nodeCounter <- get
      (leftTree, leftNodeCounter) <- lift $ runStateT (go left) nodeCounter
      (rightTree, rightNodeCounter) <- lift $ runStateT (go right) (leftNodeCounter + 1)
      put rightNodeCounter
      return (Fork leftTree leftNodeCounter rightTree)

tree = Fork (Fork (Leaf ()) () (Leaf ())) () (Leaf ())

-- GHCi> numberAndCount (Leaf ())
-- (Leaf 1,1)
-- GHCi> numberAndCount (Fork (Leaf ()) () (Leaf ()))
-- (Fork (Leaf 1) 2 (Leaf 3),2)

-- go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
-- go (Leaf x) = do
--       -- получаем текущее состояние счётчика нумерации для нашего листа
--       n <- get
--       -- увеличиваем его на 1
--       modify (+ 1)
--       -- поскольку это лист, увеличиваем счётчик листов в логе на 1
--       lift $ tell (Sum 1)
--       return $ Leaf n

-- go (Fork l x r) = do
--       -- обходим левое поддерево
--       l' <- go l
--       -- запоминаем значение для вершины
--       n <- get
--       modify (+ 1)
--       -- поскольку вершина это не лист, счётчик листов в логе не увеличиваем
--       -- обходим правое поддерево
--       r' <- go r
--       return $ Fork l' n r'