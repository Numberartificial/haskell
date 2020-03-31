data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

addNode :: Ord a => a -> BinaryTree a -> BinaryTree a
addNode b Leaf = Node Leaf b Leaf
addNode b (Node l d r)
  | b == d = Node l d r
  | b < d = Node (addNode b l) d r
  | b > d = Node l d (addNode b r)

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

prefix :: (Eq a) => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x1:xs1) (x2:xs2) = x1 == x2 && prefix xs1 xs2

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf s t@(x2:xs2) = prefix s t || isSubsequenceOf s xs2

data PersonInvalid = NameEmpty | AgeTooLow deriving(Eq, Show)

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay = undefined

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))