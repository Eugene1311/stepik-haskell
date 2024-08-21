import Control.Applicative

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

instance Functor PrsEP where
  fmap func prsEP = PrsEP $ \n -> \str -> fmap (fmap (\(a, s) -> (func a, s))) (runPrsEP prsEP n str)

instance Applicative PrsEP where
  pure a = PrsEP $ \n -> \str -> (n, Right (a, str))
  pf <*> px = PrsEP $ \p -> \s -> case runPrsEP pf p s of
    (p1, Left e) -> (p1, Left e)
    (p1, Right (f, str1)) -> runPrsEP (f <$> px) p1 str1

instance Alternative PrsEP where
  empty = PrsEP $ \p -> \_ -> (p, Left "pos " ++ show p ++ ": empty alternative")
  prsEP1 <|> prsEP2 = PrsEP $ \p -> \s -> case runPrsEP prsEP1 p s of
    (p1, Right (x, str1)) -> (p1, Right (x, str1))
    (p1, Left e1) -> case runPrsEP prsEP2 p s of
      (p2, Right (x, str2)) -> (p2, Right (x, str2))
      (p2, Left e2) -> if p2 > p1 then (p2, Left e2)
        else (p1, Left e1)

anyEP = satisfyEP (const True)
testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c
