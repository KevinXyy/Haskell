import Data.Char
import Data.List
import Test.QuickCheck
f :: Char -> Bool
f x = x `elem` ['g', 'j', 'p', 'q', 'y']

g :: String -> Int 
g xs = length [x | x <- xs, f x  ]

h :: String -> Int
h [] = 0
h (x : xs) 
         | f x = 1 + h xs
         | otherwise = h xs



c :: String -> String
c xs = [if even (snd y) then toUpper (fst y) else (fst y) | y <- zip xs [0..length xs]]

d :: String -> String
d [] = []
d [x] = [toUpper x]
d (x:y:s) = toUpper x : y : d s        

prop_cd :: String -> Bool 
prop_cd xs = c xs == d xs 

count :: String -> Int 
count xs = length [x | x <- xs, isDigit x || isUpper x ]

countRec :: String -> Int
countRec [] = 0
countRec (x:xs) | isUpper x = 1 + countRec xs
                | isDigit x = 1 + countRec xs
                | otherwise = countRec xs


prop_count :: String -> Bool
prop_count xs = count xs == countRec xs 



cc :: Char -> String -> String
cc ch str = [if even b then ch else a | (a,b) <-  zip str [0..]]

dd :: Char -> String -> String
dd _ [] = []
dd ch [x] = "."
dd ch (x:y:xs) = (ch:y:dd ch xs) 

ff :: [Int] -> Int
ff [] = 0
ff xs = sum [x ^ 2 | x <- xs, x `mod` 3 == 0 && x `mod` 5 /= 0]

gg :: [Int] -> Int
gg [] = 0
gg (x:xs) | x `mod` 3 ==0 && x `mod` 5 /= 0 = x^2 + gg xs
               | otherwise = gg xs 