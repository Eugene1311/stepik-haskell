import Control.Applicative

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap func parser = Prs fun where
    fun s | Just (a, s) <- (runPrs parser s) = Just (func a, s)
          | Nothing <- (runPrs parser s) = Nothing

anyChr :: Prs Char
anyChr = Prs f where
    f "" = Nothing
    f (x:xs) = Just (x, xs)

-- GHCi> runPrs ((,,) <$> anyChr <*> anyChr <*> anyChr) "ABCDE"
-- Just (('A','B','C'),"DE")
-- GHCi> runPrs (anyChr *> anyChr) "ABCDE"
-- Just ('B',"CDE")
instance Applicative Prs where
  pure a = Prs (\str-> Just (a, str))
  parser1 <*> parser2 = Prs func where
    func str = do
        (g, str1) <- runPrs parser1 str
        (a, str2) <- runPrs parser2 str1
        return (g a, str2)

instance Alternative Prs where
  empty = Prs (\_ -> Nothing)
  parser1 <|> parser2 = Prs func where
    func str = (runPrs parser1 str) <|> (runPrs parser2 str)

many1 :: Prs a -> Prs [a]
many1 parser = pure (:) <*> parser <*> (many1 parser <|> pure [])

char :: Char -> Prs Char
char a = Prs f where
    f "" = Nothing
    f (x:xs) = if (x == a) then Just (x, xs) else Nothing

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

nat :: Prs Int
nat = Prs $ \str -> Just (1, str)

-- GHCi> runPrs mult "14*3"
-- Just (42,"")
-- GHCi> runPrs mult "64*32"
-- Just (2048,"")
-- GHCi> runPrs mult "77*0"
-- Just (0,"")
-- GHCi> runPrs mult "2*77AAA"
-- Just (154,"AAA")
