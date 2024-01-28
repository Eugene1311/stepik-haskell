import Control.Monad.Reader

type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithPasswords = [("user", "123456"), ("x", "hi"), ("root", "123456")]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
  users <- asks ((fmap fst) . (filter (\t -> snd t == "123456")))
  return users
