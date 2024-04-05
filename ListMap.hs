import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
  deriving (Eq,Show)

instance MapLike ListMap where
  empty = ListMap []

  lookup :: Ord k => k -> ListMap k v -> Maybe v
  lookup key listMap = mapToMaybe filtered where
    filtered = filter (\(k, _) -> k == key) (getListMap listMap)
    mapToMaybe [] = Nothing
    mapToMaybe (x:xs) = Just (snd x)

  insert :: Ord k => k -> v -> ListMap k v -> ListMap k v
  insert key value listMap = ListMap $ (getListMap listMapWithoutKey) ++ [(key, value)] where
    listMapWithoutKey = delete key listMap

  delete :: Ord k => k -> ListMap k v -> ListMap k v
  delete key listMap = ListMap $ filter (\(k, _) -> k /= key) (getListMap listMap)
