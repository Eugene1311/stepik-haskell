import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Data.Char

logFirstAndRetSecond :: WriterT String
                        (Reader [String])
                        String
logFirstAndRetSecond = do
        logs <- lift $ asks head
        tell logs
        res <- lift $ asks (map toUpper . head . tail)
        return res
