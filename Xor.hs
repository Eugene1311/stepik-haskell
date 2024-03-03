import Data.Monoid

newtype Xor = Xor { getXor :: Bool }
  deriving (Eq,Show)

instance Semigroup Xor where
  a <> b = Xor $ a /= b

instance Monoid Xor where
  mempty = Xor False
  mappend = (<>)