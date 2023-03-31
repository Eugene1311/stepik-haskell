import Control.Applicative (ZipList(ZipList), getZipList)
import Data.List

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

infixl 5 >$<
(>$<) :: (a -> b) -> [a] -> [b]
-- func >$< arr = getZipList $ fmap func (ZipList arr)
(>$<) = (<$>)

infixl 5 >*<
(>*<) :: [(a -> b)] -> [a] -> [b]
list >*< arr = getZipList $ ZipList(list) <*> (ZipList arr)
