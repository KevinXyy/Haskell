-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)

-- module Main where

import LSystem
import Test.QuickCheck

pathExample = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#:  Go 30)

-- 1a. split
split :: Command -> [Command]
split (p :#: q) = split p ++ split q
split Sit = []
split x = [x]       -- Go Distance, Turn Angle

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join (x:xs) = x :#: join xs  

-- 1c. equivalent
-- equivalent 
equivalent :: Command -> Command -> Bool
equivalent p q = split p == split q 

-- 1d. testing join and split
-- prop_split_join 
prop_split_join :: [Command] -> Bool
prop_split_join xs = split (join xs) == xs

-- prop_split
prop_split :: Command -> Bool
prop_split a =  f (split a)
        where f :: [Command] -> Bool
              f [] = True
              f [Sit] = False
              f [_ :#: _] = False
              f [x] = True
              f (x:xs) | f [x] = f xs 
                       | otherwise = False 


-- 2a. copy
copy :: Int -> Command -> Command
copy i comma = join (concat(replicate i (split comma)))

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d i = copy i (Go d :#: Turn (360 / ii) )
                where ii = fromIntegral i


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral d i deltd ang 
              | i == 0 = Sit
              | i > 0  = (spiral (d+deltd) (i-1) deltd ang) :#: (Go d :#: Turn ang)
              |otherwise = Sit


-- 4. optimise
-- Remember that Go does not take negative arguments.
optimise :: Command -> Command
optimise cd = noSit (join (change (change (split cd))))
noSit :: Command -> Command 
noSit (x :#: y) | y == Sit = x 
                | y /= Sit = x :#: noSit y 
change :: [Command] -> [Command]
change [] = []
change (x:[]) = [x]  
change (x:y:xs)
          | x == Go 0 || x == Turn 0 = change (y:xs)
          | y == Go 0 || y == Turn 0 = change (x:xs)
change ((Go a):(Go b):xs)  
          | a > 0 && b > 0  = change ((Go (a+b)): xs) 
          | a < 0 || b < 0  = error "Wrong"
change ((Turn a):(Turn b):xs)
          | otherwise =  change ((Turn (a + b) : xs))
change (x:y:xs) = x: change (y:xs) 

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x 
      where 
      f 0 = GrabPen red :#: Go 10
      f x = g (x - 1) :#: p :#: f (x - 1) :#: p :#: g (x - 1)
      g 0 = GrabPen blue :#: Go 10
      g x = f (x - 1) :#: n :#: g (x - 1) :#: n :#: f (x - 1)
      n   = Turn 60
      p   = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n 
          where
         f 0 = Go 10
         f x = f (x - 1) :#: p :#: f (x - 1) :#: n :#: n :#: f (x - 1) :#: p :#: f (x - 1)
         n   = Turn 60
         p   = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x 
    where
       l 0 = GrabPen red :#: Go 10
       l x = p :#: r (x-1) :#: f (x-1) :#: n :#: l (x-1) :#: f (x-1) :#: l (x - 1) :#: n :#: f (x-1) :#: r (x-1) :#: p 
       r 0 = GrabPen blue :#: Go 10 
       r x = n :#: l (x-1) :#: f (x-1) :#: p :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: p :#: f (x - 1) :#: l (x-1) :#: n 
       f 0 = Go 100
       f x = f (x-1)
       n   = Turn 90
       p   = Turn (-90)

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------

-- Bonus L-Systems
-- f ->  f+g++g-f--ff-g+
-- g -> -f+gg++g+f--f-g
peanoGosper :: Int -> Command 
peanoGosper x = f x 
      where 
         f 0 = GrabPen red :#: Go 10 
         f x = f (x - 1) :#: p :#: g (x - 1) :#: p :#: p :#: g (x-1) :#: n :#: f (x - 1) :#: n :#: n :#: f (x-1) :#: f (x-1) :#: n :#: g (x-1) :#: p
         g 0 = GrabPen blue :#: Go 10
         g x = n :#: f (x-1) :#: p :#: g (x-1) :#: g (x-1) :#: p :#: p :#: g (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: n :#: g (x-1)
         n = Turn 60 
         p = Turn (-60)

-- f -> f-f+f+ff-f-f+f
cross :: Int -> Command
cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x :#: n 
       where
          f 0 = Go 10 
          f x = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
          n = Turn 90
          p = Turn (-90)

branch :: Int -> Command 
branch x = g x
     where
        g 0 = Go 10 
        g x = f (x-1) :#: n :#: Branch (Branch (g (x-1)) :#: p :#: g (x-1) ) :#: f (x-1) :#: Branch ( p :#: f (x-1) :#: g (x-1)) :#: n :#: g (x-1)
        f 0 = Go 10 
        f x = f (x-1) :#: f (x-1)
        n = Turn 22.5
        p = Turn (-22.5)

thirtytwo = undefined

main :: IO ()
main = display pathExample