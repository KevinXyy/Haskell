-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)

module Tutorial8 where

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]
 
 
test2 :: Catalogue
test2 = fromList [
 ("0265091316581", ("ighland Single Malt", "75ml bottle")),
 ("0903323739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342755", ("T \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212434", ("Universal g pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = ggg ys
     where 
     ys   =   [length b | (a, (b, c)) <- xs] 
     ggg [] = 0 
     ggg (x:y:[]) = max x y 
     ggg  (x:y:gs)    = ggg ((max x y) : gs)

formatLine :: Int -> (Barcode, Item) -> String
formatLine i (a, (b,c)) | i == length b = a ++ "..." ++ b ++ "..." ++ c 
                        | i >= length b = a ++ "..." ++ b ++ concat dots ++ c 
                        | otherwise = error "number is smaller than length "
                 where dots = replicate (3 + i - length b) "."

showCatalogue :: Catalogue -> String
showCatalogue ca = show (toList ca)
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []  

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing 
listToMaybe [x] = Just x  

catMaybes :: Eq a => [Maybe a] -> [a]
catMaybes xs = concat[ maybeToList x | x <- xs, x /= Nothing]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems ba cata = [ (b,c) | (a, (b,c)) <- toList cata, a `elem` ba ] 


-- Exercise 4
 
-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 12

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
