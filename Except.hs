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

trySum :: [String] -> Except SumError Integer
trySum strings = except $ foldl foldFunc (Right 0) resultsWithSumError
  where resultsWithReadError = fmap (runExcept . tryRead) strings -- [Either ReadError Integer]
        resultsWithPositions = zipWith zipFunc [1..] resultsWithReadError -- [Either (Int, ReadError) Integer]
        zipFunc i result = case result of
                              Right x -> Right x
                              Left ex -> Left (i ,ex)
        resultsWithSumError = fmap (runExcept . (withExcept (\(i, readErr) -> SumError i readErr)) . except) resultsWithPositions -- [Except SumError Integer]
        foldFunc = \a -> \b -> ((Right (+)) <*> a <*> b)
-- todo compare with it solution
-- trySum xs = sum <$> traverse (\(i, s) -> withExcept (SumError i) $ tryRead s) (zip [1..] xs)

newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show)

instance Semigroup SimpleError where
  s1 <> s2 = Simple (getSimple s1 ++ getSimple s2)

instance Monoid SimpleError where
  mempty = Simple ""
  mappend = (<>)

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple ("[index (" ++ show i ++ ") is too large]")

-- GHCi> toSimple = runExcept . withExcept lie2se
-- GHCi> xs = [1,2,3]
-- GHCi> toSimple $ xs !!! 42
-- Left (Simple {getSimple = "[index (42) is too large]"})
-- GHCi> toSimple $ xs !!! (-2)
-- Left (Simple {getSimple = "[negative index]"})
-- GHCi> toSimple $ xs !!! 2
-- Right 3
-- GHCi> import Data.Foldable (msum)
-- GHCi> toSimpleFromList = runExcept . msum . map (withExcept lie2se)
-- GHCi> toSimpleFromList [xs !!! (-2), xs !!! 42]
-- Left (Simple {getSimple = "[negative index][index (42) is too large]"})
-- GHCi> toSimpleFromList [xs !!! (-2), xs !!! 2]
-- Right 3
