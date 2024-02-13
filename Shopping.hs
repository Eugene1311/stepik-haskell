import Data.Monoid
import Control.Monad
import Control.Monad.Writer ( Writer, writer, runWriter, tell )

type Shopping = Writer ([String], (Sum Integer)) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), (item : [], (Sum cost)))

total :: Shopping -> Integer
total shopping = getSum $ snd (snd (runWriter shopping))

items :: Shopping -> [String]
items shopping = fst (snd (runWriter shopping))
