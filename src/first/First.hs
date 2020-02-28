module First where
import Data.Array.Lens ()
import Data.Char

main :: IO ()
main = putStrLn "this is a new world"

class Purity a where
  evaluate ::  a -> IO ()
  
  evaluate a = putStrLn "without effect"
  
  
data ReferentialTransparency = Identity

instance Purity ReferentialTransparency

b = ""

data Tup = R | W
instance Eq Tup where
 R == R = True
 W == W = True
 _ == _ = False
 
instance Show Tup where
  show R = "R"
  show W = "W"
 
data Trip a = Trip Tup a
instance Eq a => Eq (Trip a) where
  (==) (Trip t a) (Trip t' a') = t == t' && a == a'
  
f 1 = True
 
newtype UName = UName String
newtype UAge = UAge Int
data User = RegisterUser UName UAge
          | Unregister
          
printUser :: User -> IO ()
printUser Unregister = putStrLn "no"        
printUser (RegisterUser (UName name) (UAge age)) = putStrLn $ name ++ show age

a = [x^2 | x <- [1..10], odd x]
