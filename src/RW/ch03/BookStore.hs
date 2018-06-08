-- file ch03/BookStore.hs
module BookStore
where
import Test.QuickCheck.Test
import Data.List
import Data.Ord
import Control.Monad

data BookInfo = Book Int String [String]
  deriving (Show)

data MagzineInfo = MagzingInfo Int String [String]
  deriving (Show)

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

  --通配符模式匹配
 --如果在匹配模式中我们不在乎某个值的类型，那么可以用下划线字符 “_” 作为符号来进行标识，它也叫做*通配符*。它的用法如下。

nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors

myInfo = Book 9780135072455 "Algebra of Programming"
              ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

myNot True = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList []  = 0

goodExample (x:xs) = x + goodExample xs
goodExample _      = 0

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

customer = Customer 1 "yqd" ["haha"]

customer2 = Customer {
  customerID = 2,
  customerName = "haha",
  customerAddress = []
                     }

data List a = Cons a (List a)
  | Nil
  deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
  | EmptyTree

toList (Cons a (xs)) = a:(toList xs)
toList Nil = []


-- Exercises --

--1--
myLength (x:xs) = 1 + (myLength xs)
myLength [] = 0
testMyLength :: [Integer] -> Bool
testMyLength xs = myLength xs == length xs

testLength = quickCheck (testMyLength)

mean :: [Float] -> Float
mean xs = sum xs / (fromIntegral . myLength $ xs)

palindrome :: String -> String
palindrome s = s ++ reverse s

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (comparing length)

sortByLength' :: [[a]] -> [[a]]
sortByLength' = sortBy compareByLength
  where compareByLength s t
          | length s > length t = GT
          | length s == length t = EQ
          | length s < length t = LT

intersperse' :: a -> [[a]] -> [a]
intersperse' a s@(x1:x2:xs) = foldl (\l r -> l ++ (a : r)) (head s) (tail s)
intersperse' _ s = join s

treeHeight :: Tree a -> Integer
treeHeight (Node a tl tr) = 1 + max (treeHeight tl) (treeHeight tr)
treeHewight EmptyTree = 0

data Point t = Point t t
data Direction = MyLeft | MyRight | MyAligned
 deriving (Show)

leftOrRight :: (Num a, Ord a)=>(Point a) -> (Point a) -> (Point a) -> Direction
leftOrRight (Point ax ay) (Point bx by) (Point cx cy)
 | dp > 0 = MyLeft
 | dp < 0 = MyRight
 | dp == 0 = MyAligned
    where
      dp = ((ay - by)*(cx - bx)) - ((bx - ax)*(cy - by))

