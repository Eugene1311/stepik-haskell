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

instance Applicative OddC where
    pure a = Un a
    (Un func) <*> a = fmap func a
    (Bi func1 func2 oddC) <*> a = concat3OC (fmap func1 a) (fmap func2 a) (oddC <*> a)

instance Monad OddC where
    return a = Un a
    (Un a) >>= func = func a
    (Bi a b oddC) >>= func = concat3OC (func a) (func b) (oddC >>= func)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
cntInf = Bi 'A' 'B' cntInf

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')

tst4 = Bi 10 20 (Un 30)
tst5 = Bi 1 2 (Bi 3 4 (Un 5))
-- GHCi> concat3OC tst1 tst2 tst3
-- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a1) (Un a2) oddC3 = Bi a1 a2 oddC3
concat3OC (Un a1) (Bi a2 b2 oddC2) oddC3 = Bi a1 a2 $ concat3OC (Un b2) oddC2 oddC3
concat3OC (Bi a1 b1 oddC1) oddC2 oddC3 = Bi a1 b1 $ concat3OC oddC1 oddC2 oddC3

-- GHCi> concatOC $ Bi tst1 tst2 (Un tst3)
-- Bi tst1 tst2 (Un tst3) == Bi (Bi 'a' 'b' (Un 'c')) (Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))) (Un (Bi 'i' 'j' (Un 'k')))
-- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a b oddC) = concat3OC a b (concatOC oddC)
