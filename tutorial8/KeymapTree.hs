-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0 
depth (Node _ _ left right) = 1 + (depth left `max` depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf  = []
toList (Node k a left right) = toList left ++ [(k,a)] ++ toList right


-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right  
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f 
    where 
      f Leaf = Nothing 
      f (Node k v left right) | key == k = Just v 
                              | key <= k = f left
                              | otherwise = f right 
prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList []  = Leaf
fromList ((key, aa):xs) = foldr (uncurry set) (Node key aa Leaf Leaf) xs 


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 13

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key = f 
   where f Leaf = Leaf 
         f (Node k v left right) | k < key = Node k v (f left) (f right)
                                 | otherwise = f left

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key = f 
    where f Leaf = Leaf 
          f (Node k v left right) | k > key = Node k v (f left) (f right)
                                  | otherwise = f right  

-- Exercise 14

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf Leaf = Leaf 
merge Leaf x = x 
merge x Leaf = x 
merge (Node k1 v1 l1 r1) (Node k2 v2 l2 r2) | k1 <= k2 = Node k2 v2 (merge (filterLT k2 (Node k1 v1 l1 r1)) l2) (merge (filterGT k2 (Node k1 v1 l1 r1)) r2)
                                            | k1 > k2 = Node k1 v1 (merge (filterLT k1 (Node k2 v2 l2 r2)) l1) (merge (filterGT k1 (Node k2 v2 l2 r2)) r1)

prop_merge :: Ord k => Eq a => Keymap k a -> Keymap k a -> Bool 
prop_merge k1 k2 = nub (toList (merge k1 k2)) == (nub.toList) k1 ++ (nub.toList) k2
-- Exercise 15

del :: Ord k => k -> Keymap k a -> Keymap k a
del key Leaf = Leaf 
del key (Node k v left right) | k == key = merge left right 
                              | k < key = Node k v left (del key right)
                              | k > key = Node k v (del key left) right

-- Exercise 16

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select pred Leaf = Leaf 
select pred (Node k v left right) | pred v    = Node k v (select pred left) (select pred right)
                                  | otherwise = merge (select pred left) (select pred right)         

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary