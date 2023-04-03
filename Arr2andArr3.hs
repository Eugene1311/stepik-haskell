newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap g (Arr2 f) = Arr2 (\x -> \y -> g(f x y))

instance Functor (Arr3 e1 e2 e3) where
  fmap g (Arr3 f) = Arr3 (\x -> \y -> \z -> g(f x y z))

-- (Arr2 (\x -> \y -> (\c -> x+y-c)) <*> Arr2 ((\a -> \b) -> c)) // a b
-- GHCi> getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
-- -1
-- GHCi> getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
-- -15
instance Applicative (Arr2 e1 e2) where
  pure f = Arr2 (\x -> \y -> f)
  (Arr2 g) <*> (Arr2 f) = Arr2 (\x -> \y -> (g x y) (f x y))

instance Applicative (Arr3 e1 e2 e3) where
  pure f = Arr3 (\x -> \y -> \z -> f)
  (Arr3 g) <*> (Arr3 f) = Arr3 (\x -> \y -> \z -> (g x y z) (f x y z))

func :: (c -> d) -> (a -> b -> c) -> a -> b -> d
func f g = \x -> \y -> f(g x y)
