module Tree where

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
