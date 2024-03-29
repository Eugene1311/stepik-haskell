infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand $ expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand $ expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand $ expand e1 :*: expand e2
expand e = e

-- expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
-- (Val 1 :+: Val 2) :*: (Val 4 :+: Val 5) :+: Val 3 :*: (Val 4 :+: Val 5)

-- expand $ Val 1 :*: (Val 2 :*: (Val 3 :+: Val 4))