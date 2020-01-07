-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | even x = x `div` 2 : halveEvensRec xs
                   | otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) 
        |lo <= x && hi >= x = x : inRangeRec lo hi xs
        |otherwise = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs 


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x: xs) | x > 0 = 1 + countPositivesRec xs 
                          | otherwise = 0 + countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = (digitToInt x) * multDigitsRec xs
                     | otherwise =1 * multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs = if [snd x | x <- xs, fst x == ch] == [] then ch else head( [snd x | x <- xs, fst x == ch] )

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch []= ch
lookUpRec ch (x:xs) | fst x == ch = snd x
                    | otherwise = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k 


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = the[snd x | x <- makeKey k, fst x == ch]
    where 
    the[c] = c


-- 7.

normalize :: String -> String
normalize [] = []
normalize (x:xs) |isNumber x = x : normalize xs
                 |isAlpha x  = toUpper x : normalize xs
                 |otherwise  = normalize xs 


encipherStr :: Int -> String -> String
encipherStr k str = [encipher k x | x <- normalize str ]


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey str = [(snd x, fst x) | x <- str] 

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) = (snd x, fst x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey str = reverseKey str == reverseKeyRec str


-- 9.

decipher :: Int -> Char -> Char
decipher k ch = the[fst x | x <- makeKey k, snd x == ch]
    where 
    the[c] = c 

decipherStr :: Int -> String -> String
decipherStr k [] = []
decipherStr k (x:xs) | x `elem` ['A'..'Z'] = decipher k x : decipherStr k xs
                     | isNumber x = x : decipherStr k xs
                     | x ==' ' = x : decipherStr k xs
                     | otherwise = decipherStr k xs

-- Optional Material
-- =================


-- 10.

a :: String -> [String]
a str= [drop x str | x <- [0..length str]]  
contains :: String -> String -> Bool
contains str substr = or[isPrefixOf substr x | x <-(a str)]


-- 11.

candidates :: String -> [(Int, String)]
candidates str =[x | x <- zip [f|f <-[0..26]] [s|s <- [(decipherStr k str) | k <- [0..26]]], contains (snd x) "AND" || contains (snd x) "THE"]



-- 12.
lstr :: String -> String
lstr str| length str `mod` 5 ==0  = str
        | otherwise = str ++ drop (length str `mod` 5) "XXXXX"
splitEachFive :: String -> [String]
splitEachFive xs = [take 5 (drop x (lstr xs))|x <- [0, 5.. length (lstr xs)-5]]

prop_transpose :: String -> Bool
prop_transpose xs = transpose (transpose (splitEachFive xs )) ==splitEachFive xs 


-- 13.
add :: [String] -> String
add [] = []
add (x:xs) = x ++  add xs
encrypt :: Int -> String -> String
encrypt k str = add (transpose (splitEachFive (encipherStr k str))) 


-- 14.
splitr ::String -> [String]
splitr xs  = [take (length xs `div` 5) (drop x xs) | x <- [0, (length xs `div` 5 )..(length xs - length xs `div` 5)] ]
decrypt :: Int -> String -> String
decrypt k str = decipherStr k take [x|x <- add (transpose (splitr str))]
