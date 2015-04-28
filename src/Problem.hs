module Problem () where

import Test.QuickCheck

prop_problem1 :: [Int] -> Property
prop_problem1 xs = not (null xs) ==> head (reverse xs) == last xs

problem1 :: [a] -> a
problem1 = last
