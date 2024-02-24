-- GHCi> decode one hundred twenty three as a number
-- 123
-- GHCi> decode one hundred twenty one as a number
-- 121
-- GHCi> decode one hundred twenty as a number
-- 120

decode:: (Int -> r) -> r
decode cont = cont 0
as x cont= cont x
a = const
number = id

square:: Int -> (Int -> r) -> r
square x cont = cont(x^2)

add:: Int -> Int -> (Int -> r) -> r
add x y cont = cont (x + y)

multiply:: Int -> Int -> (Int -> r) -> r
multiply x y cont = cont (x * y)

one = add 1
two = add 2
three = add 3
seventeen = add 17
twenty = add 20
hundred = multiply 100
thousand = multiply 1000

-- decode $ one $ number
-- \0 -> (\0 -> 1) * 1
