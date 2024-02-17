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

tryRead :: Read a => String -> Except ReadError a
tryRead str | null str = throwE EmptyInput
            | null result = throwE (NoParse str)
            | (not . null) $ snd (result !! 0) = throwE (NoParse str)
            | otherwise = return $ fst (result !! 0)
                where result = reads str

tryIncrementEven :: Int -> Except String Int
tryIncrementEven x = if even x then return (x + 1) else throwE "Number is odd"

data SumError = SumError Int ReadError
  deriving Show

trySum :: [String] -> Except ReadError Integer
trySum strings = except $ foldl foldFunc (Right 0) results
  where results = fmap (runExcept . tryRead) strings -- [Either ReadError Integer]
        resultsWithPositions = zip [1..] results
        foldFunc = \a -> \b -> ((Right (+)) <*> a <*> b)

-- withExcept :: (e -> e') -> Except e a -> Except e' a