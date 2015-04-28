module Problem () where

import Test.QuickCheck
import Control.Monad

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

prop_problem3 :: [Int] -> Int -> Property
prop_problem3 xs n = not (null xs)
                  && n > 0
                  && n <= length xs ==> (problem3 xs n) `elem` map Just xs

problem3 :: [a] -> Int -> Maybe a
problem3 xs n
       | n <= 0        = Nothing
       | n > length xs = Nothing
       | otherwise     = Just $ xs !! (n-1)
