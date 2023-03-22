data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
    fmap _ (Error err) = (Error err)
    fmap g (Ok x) = Ok (g x)

instance Applicative Result where
    pure = Ok
    (Error err) <*> _ = (Error err)
    (Ok f) <*> m = fmap f m

instance Foldable Result where
    foldr f ini (Ok x) = f x ini

-- traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
instance Traversable Result where
    traverse f (Error err) = pure (Error err)
    traverse f (Ok x) = Ok <$> f x
