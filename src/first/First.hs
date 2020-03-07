module First where
import Data.Array.Lens ()
import Data.Char
import Data.Time

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

myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = foldr (\a b -> p a || b) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny (== x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy c (x:x1:[]) = max x x1
                          where max a b = if c a b == GT then a else b
myMaximumBy c (x:x1:xs) = max (max x x1) $ myMaximumBy c xs
                          where max a b = if c a b == GT then a else b
myMaximumBy c (x:[]) = x
myMaximumBy _ _ = undefined

strange = length $ take 2 $ take 4 ([1,2] ++ undefined)

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

fibs = 1 : scanl (+) 1 fibs

class This a where
  doThis :: a -> bool

instance This Int where
  doThis a = a > 10

assertFalse =  myAny even [xs| xs <- [1..10], odd xs]


