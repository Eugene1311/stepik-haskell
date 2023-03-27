data Nat = Zero | Suc Nat
    deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero b = b
add a Zero = a
add (Suc a) (Suc b) = Suc(Suc(add a b))
-- add (Suc x) y = add x (Suc y) // even better

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul (Suc a) (Suc b) = add (Suc b) (mul a (Suc b))

fac :: Nat -> Nat
fac n = facTailRecursive n (Suc Zero)

facTailRecursive :: Nat -> Nat -> Nat
facTailRecursive Zero acc = acc
facTailRecursive (Suc n) acc = facTailRecursive n (mul (Suc n) acc)
