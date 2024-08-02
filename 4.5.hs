import Control.Monad.Except
import Data.Traversable (fmapDefault, foldMapDefault)
import Control.Applicative (liftA3)

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Fork l x r) = liftA3 Fork (traverse f l) (f x) (traverse f r)

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead str | null str = throwError EmptyInput
            | null result = throwError (NoParse str)
            | (not . null) $ snd (result !! 0) = throwError (NoParse str)
            | otherwise = return $ fst (result !! 0)
                where result = reads str

treeSum :: Tree String -> Either ReadError Integer
treeSum tree = fmap (foldr1 (+)) (traverse tryRead tree)

tree = Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
oopsTree = Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")

-- GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
-- Left (NoParse "oops")
-- GHCi> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
-- Right 34
