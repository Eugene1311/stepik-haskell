data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch left x right) = foldr f (f x (foldr f ini right)) left

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch left x right)) = f x (foldr f (foldr f ini (PreO right)) (PreO left))

instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch left x right)) = foldr f (foldr f (f x ini) (PostO right)) (PostO left)

instance Foldable Levelorder where
    foldr f ini (LevelO Nil) = ini
    foldr f ini (LevelO tree) = foldr f ini (levelOrderTravers [tree] [])

getValue :: Tree a -> [a]
getValue Nil = []
getValue (Branch l v r) = [v]

getLeft :: Tree a -> [Tree a]
getLeft Nil = []
getLeft (Branch Nil v r) = []
getLeft (Branch l v r) = [l]

getRight :: Tree a -> [Tree a]
getRight Nil = []
getRight (Branch l v Nil) = []
getRight (Branch l v r) = [r]

levelOrderTravers :: [Tree a] -> [a] -> [a]
levelOrderTravers [] acc = acc
levelOrderTravers trees acc = levelOrderTravers nextTrees nextAcc
    where
        nextTrees = concatMap (\tree -> getLeft tree ++ getRight tree) trees
        nextAcc = acc ++ (concatMap getValue trees)
        

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
