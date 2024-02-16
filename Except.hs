import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) arr index = process arr index index
  where process arr currIndex index = if currIndex < 0 then throwE ErrNegativeIndex
                                        else if null arr then throwE (ErrIndexTooLarge index)
                                        else if currIndex == 0 then return (arr !! 0)
                                        else process (drop 1 arr) (currIndex - 1) index

data ReadError = EmptyInput | NoParse String
  deriving Show

-- tryRead :: Read a => String -> Except ReadError a
-- tryRead str = except (read str :: a)
-- tryRead str = (read str) `catchE` handler
--   where handler _ = except $ Left (NoParse str)

-- read' :: Read a => String -> Int