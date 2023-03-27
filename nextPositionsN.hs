type Board = [[Int]]

-- nextPositions :: Board -> [Board]
nextPositions :: [[Int]] -> [[[Int]]]
nextPositions x = [[(t:xs)] | xs <- x, t <- [1, 2, 3]]

-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN :: [[Int]] -> Int -> ([[Int]] -> Bool) -> [[[Int]]]
nextPositionsN b n pred = filter pred x
    where x = if n < 0 then [] else nextPositionsNHelper [b] n

-- nextPositionsNHelper :: [Board] -> Int -> [Board]
nextPositionsNHelper :: [[[Int]]] -> Int -> [[[Int]]]
nextPositionsNHelper boards 0 = boards
nextPositionsNHelper boards n = nextPositionsNHelper (boards >>= nextPositions) (n - 1)
