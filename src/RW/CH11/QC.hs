-- file: CH11/QC-basic.hs

module RW.CH11.QC
where
import Test.QuickCheck
import Data.List
import Control.Monad

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ls ++ (x:qsort rs)
  where ls = filter (<x) xs
        rs = filter (>=x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

quickCheckPropId = quickCheck (prop_idempotent :: [Int] -> Bool)

prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs         =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys       =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)


prop_sort_model xs      = sort xs == qsort xs

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)

data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)

instance Arbitrary Ternary where
    arbitrary     = elements [Yes, No, Unknown]

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
--     arbitrary = do
--         x <- arbitrary
--         y <- arbitrary
--         return (x, y)

-- instance Arbitrary Doc where
--     arbitrary = do
--         n <- choose (1,6) :: Gen Int
--         case n of
--              1 -> return Empty

--              2 -> do x <- arbitrary
--                      return (Char x)

--              3 -> do x <- arbitrary
--                      return (Text x)

--              4 -> return Line

--              5 -> do x <- arbitrary
--                      y <- arbitrary
--                      return (Concat x y)

--              6 -> do x <- arbitrary
--                      y <- arbitrary
--                      return (Union x y)

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

empty :: Doc
empty = Empty
(<>)  :: Doc -> Doc -> Doc
(<>) = undefined



prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty


-- prop_hcat xs = hcat xs == glue xs
--     where
--         glue []     = empty
--         glue (d:ds) = d <> glue ds

-- prop_punctuate s xs = punctuate s xs == intersperse s xs

-- prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
--     where
--         combine []           = []
--         combine [x]          = [x]

--         combine (x:Empty:ys) = x : combine ys
--         combine (Empty:y:ys) = y : combine ys
--         combine (x:y:ys)     = x `Concat` y : combine ys
