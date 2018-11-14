
import Control.Concurrent
import qualified Data.Map as M

-- Using MVar as a shared state to control access to data values
--

type Name = String
type PhoneNumber = String
type PhoneBook = M.Map Name PhoneNumber

-- free of data races.
newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  m <- newMVar M.empty
  return (PhoneBookState m)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  phonebook <- takeMVar m
  putMVar m (M.insert name number phonebook)

-- A classic mistake is to forget to "initialize" the mvar
-- once you have taken it; because if you manage to take something out
-- from it, then it will be empty and the next attempt will hang in that
-- operation.
lookup' :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup' (PhoneBookState m) name = do
  phonebook <- takeMVar m
  putMVar m phonebook
  return (M.lookup name phonebook)

main :: IO ()
main = do
  s <- new
  sequence_ [insert s ("name" ++ show n) (show n) | n <- [1..10000]]
  lookup' s "name999" >>= print
  lookup' s "name99" >>= print
  lookup' s "unknown" >>= print

