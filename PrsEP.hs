newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

-- charEP c = satisfyEP (== c)
-- GHCi> runPrsEP (charEP 'A') 0 "ABC"
-- (1,Right ('A',"BC"))
-- > runPrsEP (charEP 'A') 41 "BCD"
-- (42,Left "pos 42: unexpected B")
-- > runPrsEP (charEP 'A') 41 ""
-- (42,Left "pos 42: unexpected end of input")
satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP f = PrsEP func where
    func position "" = (position + 1, Left  ("pos " ++ show (position + 1) ++ ": unexpected end of input"))
    func position (y:ys) = (nextPosition, if (f y) then Right (y, ys) else Left  ("pos " ++ show nextPosition ++ ": unexpected " ++ [y])) where
        nextPosition = position + 1

-- GHCi> parseEP (charEP 'A') "ABC"
-- Right ('A',"BC")
-- GHCi> parseEP (charEP 'A') "BCD"
-- Left "pos 1: unexpected B"
-- GHCi> parseEP (charEP 'A') ""
-- Left "pos 1: unexpected end of input"
charEP :: Char -> PrsEP Char
charEP c = satisfyEP (== c)
-- charEP ch = PrsEP func where
--     func x "" = (x + 1, Left  ("pos " ++ show (x + 1) ++ ": unexpected end of input"))
--     func x (y:ys) = (x + 1, if (ch == y) then Right (y, ys) else Left  ("pos " ++ show (x + 1) ++ ": unexpected end of input"))
