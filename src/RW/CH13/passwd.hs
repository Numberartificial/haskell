--files : CH13/passwd.hs
module Passwd
where

import Data.List
import qualified Data.Map as Map
import System.IO

data PasswdEntry = PasswdEntry{
  usernName ::String,
  password :: String,
  uid :: Integer,
  gid :: Integer,
  gecos :: String,
  homeDir :: String,
  shell :: String}
  deriving (Eq, Ord)

instance Read PasswdEntry where
  readsPrec _ value =
    case split ':' value of
