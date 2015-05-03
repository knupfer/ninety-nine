module Main where

import Test.Invariant
import Test.Tasty
import Test.Tasty.QuickCheck
import Control.Arrow
import Data.List

main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ p "P  1"   $ (>1) . length &> p1 <~~ (tail          :: [Int] -> [Int])
    , p "P  2"   $ (>2) . length &> p2 <~~ (tail          :: [Int] -> [Int])
    , p "P  3"     prop_p3
    , p "P  4"   $ p4                  <~~ (reverse       :: [Int] -> [Int])
    , p "P  5.1" $ involutory              (p5            :: [Int] -> [Int])
    , p "P  5.2" $ length              <~~ (p5            :: [Int] -> [Int])
    , p "P  5.3" $ reverse             @~> (p5            :: [Int] -> [Int])
    , p "P  6"   $ p6                  <~~ (reverse       :: [Int] -> [Int])
    , p "P  8.1" $ deflating               (p8            :: [Int] -> [Int])
    , p "P  8.2" $ p8                  <?> (reverse       :: [Int] -> [Int])
    , p "P  8.3" $ idempotent              (p8            :: [Int] -> [Int])
    , p "P  8.4" $ p8                  <=> (p8'           :: [Int] -> [Int])
    , p "P  9.1" $ deflating               (p9            :: [Int] -> [[Int]])
    , p "P  9.2" $ reverse . p9        <=> (p9 . reverse  :: [Int] -> [[Int]])
    , p "P 10.1" $ deflating               (p10           :: [Int] -> [(Int,Int)])
    , p "P 10.1" $ reverse . p10       <=> (p10 . reverse :: [Int] -> [(Int,Int)])
    , p "P 11"   $ deflating               (p11           :: [Int] -> [Runner Int])
    , p "P 11"   $ p11 . reverse       <=> (reverse . p11 :: [Int] -> [Runner Int])
    ]

p :: Testable a => TestName -> a -> TestTree
p = testProperty

p1 :: [a] -> a
p1 = last

p2 :: [a] -> Maybe a
p2 xs
       | length xs > 1 = Just . head . drop 1 $ reverse xs
       | otherwise     = Nothing

prop_p3 :: [Int] -> Int -> Property
prop_p3 xs n = not (null xs)
                  && n > 0
                  && n <= length xs ==> p3 xs n `elem` map Just xs

p3 :: [a] -> Int -> Maybe a
p3 xs n
       | n <= 0        = Nothing
       | n > length xs = Nothing
       | otherwise     = Just $ xs !! (n-1)

prop_p4 :: [Int] -> Property
prop_p4 xs = p4 xs === length xs

p4 :: [a] -> Int
p4 (_:xs) = 1 + p4 xs
p4 [] = 0

p5 :: [a] -> [a]
p5 (x:xs) = p5 xs ++ [x]
p5 _ = []

p6 :: Eq a => [a] -> Bool
p6 = reverse <=> id

data Lisp a = Leaf a | Branch [Lisp a]

p7 :: Lisp a -> [a]
p7 (Leaf a) = [a]
p7 (Branch xs) = concatMap p7 xs

p8 :: Eq a => [a] -> [a]
p8 (x:xs) = x: p8 (dropWhile (==x) xs)
p8 [] = []

p8' :: Eq a => [a] -> [a]
p8' = map head . group

p9 :: Eq a => [a] -> [[a]]
p9 = group

p10 :: Eq a => [a] -> [(Int,a)]
p10 = map (length &&& head) . group

data Runner a = Single a | Multiple (Int, a) deriving Eq

p11 :: Eq a => [a] -> [Runner a]
p11 = map (toRunner . (length &&& head)) . group
  where toRunner (n, x) | n <= 1    = Single x
                        | otherwise = Multiple (n, x)
