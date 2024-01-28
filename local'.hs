local' :: (r -> r') -> Reader r' a -> Reader r a
local' f rdr = Reader(\env -> runReader rdr (f (env)))
