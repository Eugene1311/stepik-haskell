newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

-- GHCi> fmap (^2) $ Cmps3 [[[1],[2,3,4],[5,6]],[],[[7,8],[9,10,11]]]
-- Cmps3 {getCmps3 = [[[1],[4,9,16],[25,36]],[],[[49,64],[81,100,121]]]}

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
    -- fmap func (Cmps3 cmps) = Cmps3 (fmap (\g -> fmap (\h -> fmap func h) g) cmps)
    fmap func (Cmps3 cmps) = Cmps3 ((fmap . fmap . fmap) func cmps)

-- fmap 1 :: (a -> b) -> functor1 a -> functor1 b
-- fmap 2 :: (a -> b) -> functor2 a -> functor2 b

-- fmap . fmap :: (((a -> b) -> functor1 a) -> functor1 b) -> functor2 a -> functor2 b
