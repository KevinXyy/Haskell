-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)

-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x <- xs, even x]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo, x <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0 ]



-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product ([digitToInt  x | x <- str, x `elem` ['0'..'9']])
countDigits :: String -> Int
countDigits str = length [x | x <- str, isNumber x]

prop_multDigits :: String -> Bool
prop_multDigits xs =  multDigits xs <= 9 ^ (countDigits xs)
-- Have some problems


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise s = toUpper (head s) : [ toLower x | x <- tail s] 


-- 6. title

lowercase :: String -> String
lowercase xs = [ toLower x | x <- xs]

-- List-comprehension version
title :: [String] -> [String]
title xs = capitalise (head xs) :([if length s >=4 then capitalise s else lowercase s | s <- tail xs])   


-- 7. signs

sign :: Int -> Char
sign i | i >= 1 && i <= 9 = '+' 
       | i >= -9 && i <= -1 = '-' 
       | i == 0 = '0'
       | otherwise = error "e"
signs :: [Int] -> String
signs xs = [sign x | x <- xs, x >= -9 && x <= 9]


-- 8. score

score :: Char -> Int
score x | x `elem` ['a','e','i','o','u']  = 2
        | x `elem` ['A','E','I','O','U'] = 3
		| x `elem` ['a'..'z'] = 1
		| x `elem` ['A'..'Z'] = 2
		| otherwise = 0

totalScore :: String -> Int
totalScore xs = product [score x | x <- xs,  x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1

-- Tutorial Activity
-- 10. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = round (sum [fromIntegral x * 0.9| x <- prices, fromIntegral x * 0.9 <= 19900, x >= 0 ]) 

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [ x | x <- xs, x >= 0]  

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ a | a <- words, len == length a, letter `elem` a, letter == a !! pos ]


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [snd a | a <- (zip [ x | x <- str] [0..length str - 1]), fst a == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str


-- 13. contains
a :: String -> [String]
a str= [drop x str | x <- [0..length str]]  
contains :: String -> String -> Bool
contains str substr = or[isPrefixOf substr x | x <-(a str)]

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = undefined
