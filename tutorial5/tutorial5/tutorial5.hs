-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where

import Data.List
import Data.Char
import Data.Ratio
import Test.QuickCheck

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles xs = map (* 2) xs

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (/ 100)  (map fromIntegral xs)  

-- c.
uppers :: String -> String
uppers xs = map toUpper xs 
uppersComp :: String -> String
uppersComp xs = [toUpper x | x <- xs] 
prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppersComp xs


-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter isAlpha xs

-- b.
above :: Int -> [Int] -> [Int]
above x xs = filter (>x) xs

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter eqa xs 
               where 
               eqa (a,b) = a /= b


-- d.
rmCharComp :: Char -> String -> String
rmCharComp x xs = filter (/= x) xs  


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (* 2) (filter (>3) xs ) 

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' xs = map reverse (filter equ xs )
                        where equ x = even (length x) 
prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold xs  = foldr (&&) True xs 

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs)  = x ++ concatRec xs 

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs  

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] xs = xs 
rmCharsRec gs [] = []
rmCharsRec (g:gs) xs = rmCharsRec gs (rmCharComp g xs)
              

rmCharsFold :: String -> String -> String
rmCharsFold gs xs = foldr (rmCharComp) xs gs 

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform (x:xs) = all (==x) (x:xs)

-- b.
valid :: Matrix -> Bool
valid mat | length mat >= 1 && and [length x >=1 | x <- mat ] = uniform (map length mat)
          | otherwise = False


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m | uniform (map length m) == True = length (head m)
              |otherwise = error "Not uniform" 

matrixHeight :: Matrix -> Int
matrixHeight m = length m 

plusM :: Matrix -> Matrix -> Matrix
plusM ma mb | matrixWidth ma == matrixWidth mb && matrixHeight ma == matrixHeight mb = zipWith (zipWith (+)) ma mb

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM ma mb | matrixWidth ma == matrixHeight mb = [[dotPro x y | y <- transpose mb] |x <- ma ]
             | otherwise = error "Error!!!"

dotPro :: [Rational] -> [Rational] -> Rational
dotPro a b = sum (zipWith (*) a b) 

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ a `f` b  | (a,b) <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f)  (zip xs ys)


-- -----------------------------------
-- -----------------------------------
-- -- Optional material
-- -----------------------------------
-- -----------------------------------
-- -- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f ases= map (map f) ases

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f aa bb = zipWith (zipWith f) aa bb

-- All ways of deleting a single element from a list
removes :: Eq a => [a] -> [[a]]     
removes xs = [filter (/= x) xs | x <- xs ]  

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined