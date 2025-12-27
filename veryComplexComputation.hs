import MyRWT
import Data.Char
import Data.List

logFirstAndRetSecond :: MyRWT Maybe String
logFirstAndRetSecond = do
  xs <- myAsk
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

-- GHCi> runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
-- Just ("DEFG","abc")
-- GHCi> runMyRWT logFirstAndRetSecond ["abc"]
-- Nothing

-- Реализуйте безопасную функцию veryComplexComputation, записывающую в лог через запятую первую строку четной длины и первую строку нечетной длины,
-- а возвращающую пару из второй строки четной и второй строки нечетной длины, приведенных к верхнему регистру:
-- veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- myAsk
  let partitioned = partition (even . length) xs
  case partitioned of
    ((a: b: _), (c: d: _)) -> myTell (a ++ "," ++ c) >> return (map toUpper b, map toUpper d)
    _ -> myLift Nothing

-- GHCi> runMyRWT veryComplexComputation ["abc","defg","hij"]
-- Nothing
-- GHCi> runMyRWT veryComplexComputation ["abc","defg","hij","kl"]
-- Just (("KL","HIJ"),"defg,abc")
