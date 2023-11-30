-- 2.3.8
-- Сделайте двоичное дерево

-- data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)
-- представителем класса типов Traversable таким образом, чтобы обеспечить для foldMapDefault порядок обхода «postorder traversal»:

-- GHCi> testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- GHCi> foldMapDefault (\x -> [x]) testTree
-- [1,3,2,5,4]

import Data.Traversable (foldMapDefault)
import Tree

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch left x right) = Branch (fmap f left) (f x) (fmap f right)

instance Foldable Tree where
  foldMap = foldMapDefault

-- instance Traversable Tree where
--   sequenceA = undefined

instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse f (Branch left x right) = flip <$> Branch <$> (traverse f left) <*> (traverse f right) <*> f x

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

testTree1 = Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)

testTreeOfMaybe = Branch (Branch (Branch Nil (Just 1) Nil) (Just 2) (Branch Nil (Just 3) Nil)) (Just 4) (Branch Nil (Just 5) Nil)

testTreeOfMaybe1 = Branch (Branch Nil (Just 1) Nil) (Just 2) (Branch Nil (Just 3) Nil)

-- traverse id testTreeOfMaybe1 ==
-- Branch <$> (traverse id Branch (Nil (Just 1) Nil)) <*> (Just 2) <*> (traverse id Branch (Nil (Just 3) Nil)) ==
-- Branch <$> (Branch <$> (Just Nil) <*> (Just 1) <*> (Just Nil)) <*> (Just 2) <*> (Branch <$> (Just Nil) <*> (Just 3) <*> (Just Nil)) ==
-- Branch <$> (Just (Branch Nil 1 Nil)) <*> (Just 2) <*> ((Just (Branch Nil 3 Nil)) ==
-- Just (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil))

-- Maybe (a -> Tree a -> Tree a)  =>  Maybe (Tree a -> a -> Tree a)