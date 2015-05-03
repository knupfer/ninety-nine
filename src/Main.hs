module Main where

import Test.Invariant
import Test.Tasty
import Test.Tasty.QuickCheck
import Control.Arrow
import Data.List
import System.Random

main :: IO ()
main = defaultMain $ testGroup "Tests"
 [ arity1
 , arity2
 ]

arity1 :: TestTree
arity1 =  testGroup "Arity 1"
  [ p "P  1"   $ (>1) . length &> p1 <~~ (tail          :: [Int] -> [Int])
  , p "P  2"   $ (>2) . length &> p2 <~~ (tail          :: [Int] -> [Int])
  , p "P  4.1" $ p4                  <~~ (reverse       :: [Int] -> [Int])
  , p "P  4.2" $ p4                  <=> (length        :: [Int] -> Int)
  , p "P  5.1" $ involutory              (p5            :: [Int] -> [Int])
  , p "P  5.2" $ length              <~~ (p5            :: [Int] -> [Int])
  , p "P  5.3" $ reverse             @~> (p5            :: [Int] -> [Int])
  , p "P  6"   $ p6                  <~~ (reverse       :: [Int] -> [Int])
  , p "P  7.1" $ p7 . N . map L      <=> (id            :: [Int] -> [Int])
  , p "P  7.2" $ p7 . N . map L      <?> (reverse       :: [Int] -> [Int])
  , p "P  8.1" $ deflating               (p8            :: [Int] -> [Int])
  , p "P  8.2" $ p8                  <?> (reverse       :: [Int] -> [Int])
  , p "P  8.3" $ idempotent              (p8            :: [Int] -> [Int])
  , p "P  8.4" $ p8                  <=> (p8'           :: [Int] -> [Int])
  , p "P  9.1" $ deflating               (p9            :: [Int] -> [[Int]])
  , p "P  9.2" $ reverse . p9        <=> (p9 . reverse  :: [Int] -> [[Int]])
  , p "P 10.1" $ deflating               (p10           :: [Int] -> [(Int,Int)])
  , p "P 10.2" $ reverse . p10       <=> (p10 . reverse :: [Int] -> [(Int,Int)])
  , p "P 11.1" $ deflating               (p11           :: [Int] -> [Runner Int])
  , p "P 11.2" $ p11 . reverse       <=> (reverse . p11 :: [Int] -> [Runner Int])
  , p "P 12"   $ p12                 @~> (p11           :: [Int] -> [Runner Int])
  , p "P 13"   $ p13                 <=> (p11           :: [Int] -> [Runner Int])
  , p "P 14.1" $ inflating               (p14           :: [Int] -> [Int])
  , p "P 14.2" $ reverse             <?> (p14           :: [Int] -> [Int])
  , p "P 14.3" $ monotonicIncreasing'    (p14           :: [Int] -> [Int])
  , p "P 25.1" $ length              <~~ (p25           :: [Int] -> [Int])
  , p "P 25.2" $ sort                <=> (sort . p25    :: [Int] -> [Int])
  ]

arity2 :: TestTree
arity2 = testGroup "Arity 2"
  [ p "P  3"   $ \xs x   -> maybe True (`elem`xs) (p3 (xs :: [Int]) x)
  , p "P 15.1" $ \xs n   -> p15 (reverse xs) n == reverse (p15 (xs::[Int]) n)
  , p "P 15.2" $ \xs n   -> n > 0 ==> length (p15 (xs :: [Int]) n) >= length xs
  , p "P 16.1" $ \xs n   -> length (p16 xs n) <= length (xs :: [Int])
  , p "P 16.2" $ p16               <<=>> (p16'          :: [Int] -> Int -> [Int])
  , p "P 17"   $ \xs n   -> uncurry (++) (p17 xs n) == (xs :: [Int])
  , p "P 18"   $ \xs a b -> length xs >= length (p18 (xs :: [Int]) a b)
  , p "P 19.1" $ (.) length . p19'  <<=> (length :: [Int] -> Int)
  , p "P 19.2" $ flip (p19' :: [Int] -> Int -> [Int]) 2 `cyclesWithin` 100
  , p "P 20"   $ \xs n   -> length xs >= length (p20 (xs :: [Int]) n)
  , p "P 21.1" $ \i xs n -> length xs + 1 == length (p21 (i :: Int) xs n)
  , p "P 21.2" $ \i xs n -> xs == p20 (p21 (i :: Int) xs n) n
  , p "P 22"   $ \a b    -> max 0 (b - a + 1) == length (p22 a b)
  , p "P 23.1" $ \xs n   -> not (null xs) && n >= 0 ==> n == length (p23 (xs :: [Int]) n)
  , p "P 23.2" $ \xs n   -> not (null xs) ==> all (`elem` xs) (p23 (xs :: [Int]) n)
  , p "P 24.1" $ \x y    -> x <= y ==> max 0 x == length (p24 x y)
  , p "P 24.2" $ \x y    -> x <= y ==> all (`elem` [1..y]) (p24 x y)
  ]

p :: Testable a => TestName -> a -> TestTree
p = testProperty

p1 :: [a] -> a
p1 = last

p2 :: [a] -> Maybe a
p2 xs | length xs > 1 = Just . head . drop 1 $ reverse xs
      | otherwise     = Nothing

p3 :: [a] -> Int -> Maybe a
p3 xs n | n <= 0        = Nothing
        | n > length xs = Nothing
        | otherwise     = Just $ xs !! (n-1)

p4 :: [a] -> Int
p4 (_:xs) = 1 + p4 xs
p4 [] = 0

p5 :: [a] -> [a]
p5 (x:xs) = p5 xs ++ [x]
p5 _ = []

p6 :: Eq a => [a] -> Bool
p6 = reverse <=> id

data Lisp a = L a | N [Lisp a]
p7 :: Lisp a -> [a]
p7 (L a) = [a]
p7 (N xs) = concatMap p7 xs

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

p12 :: [Runner a] -> [a]
p12 = concatMap fromRunner
  where fromRunner (Single x)        = [x]
        fromRunner (Multiple (n, x)) = replicate n x

p13 :: Eq a => [a] -> [Runner a]
p13 []     = []
p13 (x:xs) = toRunner (1,x) xs
  where toRunner (n,x') []               = [f n x']
        toRunner (n,x') (y:ys) | y == x' = toRunner (n+1,y) ys
                               | y /= x' = f n x' : toRunner (1,y) ys
        f n x' = if n > 1 then Multiple (n, x')
                          else Single x'

p14 :: [a] -> [a]
p14 = concatMap $ replicate 2

p15 :: [a] -> Int -> [a]
p15 xs n = concatMap (replicate n) xs

p16 :: [a] -> Int -> [a]
p16 xs n | n < 2 || null xs = []
         | otherwise = take (n-1) xs ++ p16 (drop n xs) n

p16' :: [a] -> Int -> [a]
p16' xs n | n < 2     = []
          | otherwise = [ i | (i,c) <- zip xs [1..], c `mod` n /= 0]

p17 :: [a] -> Int -> ([a],[a])
p17 xs n = (take n xs, drop n xs)

p18 :: [a] -> Int -> Int -> [a]
p18 xs a b = take (1+b-a) $ drop (a-1) xs

p19' :: [a] -> Int -> [a]
p19' xs n = take (length xs) $ drop (length xs + n) $ cycle xs

p20 :: [a] -> Int -> [a]
p20 xs n = take (min n (length xs) -1) xs ++ drop (max n 1) xs

p21 :: a -> [a] -> Int -> [a]
p21 i xs n = take (n-1) xs ++ i:drop (n-1) xs

p22 :: Int -> Int -> [Int]
p22 a b = [a..b]

p23 :: [a] -> Int -> [a]
p23 xs n = take n . map (xs!!) $ randomRs (0, length xs - 1) $ mkStdGen 0

p24 :: Int -> Int -> [Int]
p24 m n | m <= n = take m . nub . randomRs (1, n) $ mkStdGen 0
        | otherwise = []

p25 :: [a] -> [a]
p25 xs = map (xs!!) . take (length xs) . nub . randomRs (0, length xs - 1) $ mkStdGen 0
