sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | abs x < 10 = (abs x, 1)
              | otherwise = ( abs(rem x 10) + fst (sum'n'count $ abs(x `div` 10)), 1 + snd (sum'n'count (x `div` 10)) )