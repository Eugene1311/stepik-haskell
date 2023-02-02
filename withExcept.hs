module Demo where
import Control.Monad (liftM, ap)
import Data.Bifunctor

newtype Except e a = Except {runExcept :: Either e a} deriving (Show)

except:: Either e a -> Except e a
except = Except

instance Functor (Except e) where
    fmap = liftM

instance Applicative (Except e) where
    pure = return
    (<*>) = ap

instance Monad (Except e) where
    return a = Except (Right a)
    m >>= k =
        case runExcept m of
            Left a -> Except (Left a)
            Right b -> k b

instance Bifunctor Except where
 -- first :: (a -> b) -> p a c -> p b c
    first func m =
        case runExcept m of
            Left a -> Except (Left (func a))
            Right b -> Except (Right b)
 -- second :: (b -> c) -> p a b -> p a c
    second func m = fmap func m 

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept func m = first func m
