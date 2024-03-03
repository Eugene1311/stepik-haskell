import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

readerToState :: Reader r a -> State r a
readerToState reader = state $ \s -> (runReader reader s, s)

writerToState :: Monoid w => Writer w a -> State w a
writerToState writer = do
  s <- get
  let (x , a) = runWriter writer
  put (s `mappend` a)
  return x
