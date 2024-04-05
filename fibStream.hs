fibStream :: [Integer]
fibStream = zipWith (+) (0 : fibStream) (0 : 1 : fibStream)
-- zipWith (+) (0 : 0 : 1 : 1 : 2 : 3 : ...) (0 : 1 : 0 : 1 : 1 : 2 : ...)
