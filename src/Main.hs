module Main where

import Test.Invariant
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List

main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ testProperty "Problem 1"   $
                  (>1) . length &> problem1 <~~ (tail :: [Int] -> [Int])
    , testProperty "Problem 2"   $
                  (>2) . length &> problem2 <~~ (tail :: [Int] -> [Int])
    , testProperty "Problem 3"     prop_problem3
    , testProperty "Problem 4"   $ problem4 <~~ (reverse :: [Int] -> [Int])
    , testProperty "Problem 5.1" $ involutory   (problem5 :: [Int] -> [Int])
    , testProperty "Problem 5.2" $ length   <~~ (problem5 :: [Int] -> [Int])
    , testProperty "Problem 5.3" $ reverse  @~> (problem5 :: [Int] -> [Int])
    , testProperty "Problem 6"   $ problem6 <~~ (reverse :: [Int] -> [Int])
    , testProperty "Problem 8.1" $ deflating (problem8 :: [Int] -> [Int])
    , testProperty "Problem 8.2" $ reverse . problem8 <=> problem8 . (reverse :: [Int] -> [Int])
    , testProperty "Problem 8.3" $ idempotent (problem8 :: [Int] -> [Int])
    , testProperty "Problem 8.4" $ problem8 <=> (problem8' :: [Int] -> [Int])
    ]

problem1 :: [a] -> a
problem1 = last

problem2 :: [a] -> Maybe a
problem2 xs
       | length xs > 1 = Just . head . drop 1 $ reverse xs
       | otherwise     = Nothing

prop_problem3 :: [Int] -> Int -> Property
prop_problem3 xs n = not (null xs)
                  && n > 0
                  && n <= length xs ==> problem3 xs n `elem` map Just xs

problem3 :: [a] -> Int -> Maybe a
problem3 xs n
       | n <= 0        = Nothing
       | n > length xs = Nothing
       | otherwise     = Just $ xs !! (n-1)

prop_problem4 :: [Int] -> Property
prop_problem4 xs = problem4 xs === length xs

problem4 :: [a] -> Int
problem4 (_:xs) = 1 + problem4 xs
problem4 [] = 0

problem5 :: [a] -> [a]
problem5 (x:xs) = problem5 xs ++ [x]
problem5 _ = []

problem6 :: Eq a => [a] -> Bool
problem6 = reverse <=> id

data Lisp a = Leaf a | Branch [Lisp a]

problem7 :: Lisp a -> [a]
problem7 (Leaf a) = [a]
problem7 (Branch xs) = concatMap problem7 xs

problem8 :: Eq a => [a] -> [a]
problem8 (x:xs) = x: problem8 (dropWhile (==x) xs)
problem8 [] = []

problem8' :: Eq a => [a] -> [a]
problem8' = map head . group
