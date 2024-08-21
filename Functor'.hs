{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

class Functor' c e | c -> e where
  fmap' :: (e -> e) -> c -> c

instance (Eq a) => Functor' (Maybe a) a where
  fmap' func Nothing = Nothing
  fmap' func (Just x) = Just (func x)

instance (Eq a) => Functor' [a] a where
  fmap' func [] = []
  fmap' func (x:xs) = (func x) : (fmap' func xs)
