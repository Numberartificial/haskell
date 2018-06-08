-- file: CH13/lookup.hs

module lookup
where
import Data.List

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((thiskey, thisval):rest) =
    if key == thiskey
       then Just thisval
       else myLookup key rest
