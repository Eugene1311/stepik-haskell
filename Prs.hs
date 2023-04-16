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
    func str = do {
        (g, str1) <- runPrs parser1 str;
        (a, str2) <- runPrs parser2 str1;
        return (g a, str2)}

-- instance Alternative Prs where
--   empty = pure
--   parser1 <|> parser2 = undefined