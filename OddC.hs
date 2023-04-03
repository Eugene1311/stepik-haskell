data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
    fmap func (Un x) = Un (func x)
    fmap func (Bi x y oddC) = Bi (func x) (func y) (fmap func oddC)

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
instance Foldable OddC where
    foldr func initVal (Un x) = func x initVal
    foldr func initVal (Bi x y oddC) = func x (func y (foldr func initVal oddC))

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
instance Traversable OddC where
    traverse func (Un x) = pure Un <*> func x
    traverse func (Bi x y oddC) = Bi <$> func x <*> func y <*> (traverse func oddC)
-- traverse (\x y z -> [x + 1, 2 * y, z ^ 3]) (Bi 1 2 (Un 3)) => [(Bi 2 2 ()), (Bi 3 4)] - ??

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
cntInf = Bi 'A' 'B' cntInf
