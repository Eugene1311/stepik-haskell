newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE predicate = PrsE f where
  f "" = Left "unexpected end of input"
  f (x:xs) = if (predicate x) then Right (x, xs)
             else Left ("unexpected " ++ [x])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

-- GHCi> runPrsE (charE 'A') "ABC"
-- Right ('A',"BC")
-- GHCi> runPrsE (charE 'A') "BCD"
-- Left "unexpected B"
-- GHCi> runPrsE (charE 'A') ""
-- Left "unexpected end of input"

instance Functor PrsE where
  -- fmap :: (a -> b) -> PrsE a -> PrsE b
  fmap f parser = PrsE $ \str -> do
    (a, x) <- runPrsE parser str
    return (f a, x)

instance Applicative PrsE where
  pure a = PrsE $ \str -> Right (a, str)
  parser1 <*> parser2 = PrsE $ \str -> do
    (f, str1) <- runPrsE parser1 str
    (a, str2) <- runPrsE parser2 str1
    return (f a, str2)

anyE = satisfyE (const True)
-- GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
-- Right (('A','C'),"DE")
-- GHCi> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
-- Left "unexpected B"
-- GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
-- Left "unexpected end of input"
 