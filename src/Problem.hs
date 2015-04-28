module Problem () where

import Test.QuickCheck

prop_problem1 :: [Int] -> Property
prop_problem1 xs = not (null xs) ==> head (reverse xs) == last xs

problem1 :: [a] -> a
problem1 = last

prop_problem2 :: [Int] -> Property
prop_problem2 xs = length xs > 1 ==> Just (last $ init xs) == problem2 xs

problem2 :: [a] -> Maybe a
problem2 xs
          | length xs > 1 = Just . head . drop 1 $ reverse xs
          | otherwise     = Nothing
