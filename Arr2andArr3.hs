newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap g (Arr2 f) = Arr2 (\x -> \y -> g(f x y))

instance Functor (Arr3 e1 e2 e3) where
  fmap g (Arr3 f) = Arr3 (\x -> \y -> \z -> g(f x y z))

func :: (c -> d) -> (a -> b -> c) -> a -> b -> d
func f g = \x -> \y -> f(g x y)
