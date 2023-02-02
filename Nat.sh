data Nat = Zero | Suc Nat
    deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add a Zero = a
add Zero b = b
add (Sum a) (Sum b) = Sum(Sum(add a b))

mul :: Nat -> Nat -> Nat
mul = undefined

fac :: Nat -> Nat
fac = undefined
