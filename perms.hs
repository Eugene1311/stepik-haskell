import Data.List

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (\list -> getAllInsertions x list) (perms xs) -- permsRunner x (perm xs)

insertAtPosition :: a -> Int -> [a] -> [a]
insertAtPosition x idx [] = []
insertAtPosition x idx arr = (fst splitted ) ++ [x] ++ (snd splitted)
  where splitted = splitAt idx arr

getAllInsertions :: a -> [a] -> [[a]]
getAllInsertions x [] = [[x]]
getAllInsertions x arr = insertAtAllPositions x 0 arr

insertAtAllPositions :: a -> Int -> [a] -> [[a]]
insertAtAllPositions x idx [] = []
insertAtAllPositions x idx arr | idx > (length arr) = []
                               | otherwise = (insertAtPosition x idx arr) : (insertAtAllPositions x (idx + 1) arr)

permsRunner :: a -> [[a]] -> [[a]]
permsRunner x lists = concatMap (\list -> getAllInsertions x list) lists

----------------------

test :: (Eq a) => [a] -> [[a]]
test (x:[]) = [[x]]
test (y:[z]) = [y:[z], [z] ++ [y]]
test (x:[y, z]) = nub $ (concatMap (\arr -> [x:arr, arr ++ [x]]) (test [y, z])) ++ (concatMap (\arr -> [y:arr, arr ++ [y]]) (test [x, z]))
    -- [[x, y, z], [y, z, x], [x, z, y], [z, y, x], [y, x, z], [x, z, y], [y, z, x], [z, x, y]]

-- combine :: a -> [a] -> [[a]]
-- combine x [] = [[x]]
-- combine x [y] = [x : [y], [y] ++ [x]]
-- combine x xs = [x : xs, (head xs) : x : (tail xs), (head . head xs) : x : (tail . tail xs), ... , xs ++ [x]]

-- ghci> arr
-- [2,3,4,5]
-- ghci> (head arr) : 1 : (tail arr)
-- [2,1,3,4,5]
-- ghci> (head arr) : ((head . tail) arr) : 1 : ((tail . tail) arr)
-- [2,3,1,4,5]
-- ghci> (head arr) : ((head . tail) arr) : ((head . tail . tail) arr) : 1 : ((tail . tail . tail) arr)
-- [2,3,4,1,5]

insert' :: a -> [a] -> [a]
insert' x [] = []
insert' x arr = (head arr) : x : (tail arr)

insert'' :: [a] -> a -> [a] -> [a]
insert'' pre x [] = pre
insert'' pre x arr = pre ++ (insert' x (tail arr))

-- ghci> (head arr) : 1 : (tail arr) == insert' 1 arr
-- [2,1,3,4,5]

-- ghci> (head arr) : ((head . tail) arr) : 1 : ((tail . tail) arr) == (head arr) : (insert' 1 (tail arr))
-- [2,3,1,4,5]

getListWithTails :: [a] -> [[a]]
getListWithTails [] = []
getListWithTails arr = arr : getListWithTails (tail arr)

-- getListWithInsert :: a -> [a] -> [[a]]
-- getListWithInsert x [] = [[]]
-- getListWithInsert x arr = (insert' x arr) : ((head arr) : (insert' x (tail arr))) : ... : (getListWithInsert x (tail arr))

test' :: Int -> [Int]
test' x | x > 3 = []
        | otherwise = x:(test' (x + 1))


