data Person =
  Person { name :: String,
           age :: Int}
          deriving (Eq, Show)


data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Multi Expr Expr
  | Divide Expr Expr

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)