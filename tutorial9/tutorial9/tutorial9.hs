-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 9(11-15 Nov.)
module Tutorial9 where

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations

type FSM q = ([q], Alphabet, [q], [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      [0],
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      ['B'],
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [[0]],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> [q]
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]

states (k, _, _, _, _) = k
alph   (_, a, _, _, _) = a
start  (_, _, s, _, _) = s
final  (_, _, _, f, _) = f
trans  (_, _, _, _, t) = t


-- 2.
delta :: (Eq q) => FSM q -> [q] -> Char -> [q]
delta m s symbol = [d | (s',symbol', d) <- trans m, s' `elem` s, symbol'== symbol]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts fsm str = acceptsFrom fsm (start fsm) str

accepts' :: (Eq q) => FSM q -> String -> Bool
accepts' fsm xs | final fsm == [] = False 
               | xs == "" = or [ a `elem` (start fsm) | a <- final fsm]
accepts' (k,a,sta,f,t) (x:xs) = accepts (k,a,delta (k,a,sta,f,t) sta x,f,t) xs
 

acceptsFrom :: (Eq q) => FSM q -> [q] -> String -> Bool
acceptsFrom m q "" = or[ r `elem` final m | r <- q ]
acceptsFrom m q (x:xs) = acceptsFrom m (delta m q x) xs


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical xs = (sort.nub) xs


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m s symbol = canonical $ delta m s symbol

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m [] = []
next m xs = nub (xs ++ [ddelta m x symb | x <- xs, symb <- (alph m) ])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m [] = []
reachable m xs  | xs == (next m xs) = xs
reachable m xs = nub $ xs ++ reachable m (next m xs)

-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m xs = [x | x <- xs, or [g `elem` x | g <- final m]] 

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m xs =  concat [[(q,t, ddelta m q t) | t <- alph m] | q <- xs] 


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (dstates m , alph m , [start m], dfinal m (dstates m) ,dtrans m (dstates m))
          where dstates m = sort $ reachable m [start m]


-- Optional Material

-- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

--11.

charFSM :: Char -> FSM Bool
charFSM chr = ([False,True],[chr],[False],[True],[(False, chr, True)])

emptyFSM :: FSM ()
emptyFSM = ([],"",[],[],[])

--12.

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM a b 
          | trans a == [] && trans b == [] = ((map Left (states a) ++  map Right (states b)), alph a, map Left (start a) , map Left (final a), [])
          | trans a == [] = ((map Left (states a) ++ map Right (states b)),  alph a, map Right (start b), map Right (final b), [(Right x,ch,Right x') | (x,ch,x') <-trans b])
          | trans b == [] = ((map Left (states a) ++ map Right (states b)),  alph a, map Left (start a), map Left (final a), [(Left x,ch,Left x') | (x,ch,x') <-trans a])
          | alph a == alph b = ((map Left (states a) ++  map Right (states b)), alph a ,map Left (start a) , map Right (final b), 
          ([(Left x, ch, Left x') | (x,ch,x') <- trans a]  ++ [(Left x, ch, Right x'') | (x,ch,fi) <- trans a, fi <- final a , x'' <- start b] 
          ++ [(Right x,ch,Right x') | (x,ch,x') <-trans b]))
          | alph a /= alph b = error "not the same"

prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o =
  accepts fsm (s ++ t)
  && (accepts fsm u == (s ++ t == u))
  where
  fsm = concatFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

--13.

intFSM :: Ord q => FSM q -> FSM Int
intFSM a = ([0..length (states a) - 1], alph a, [0], [length (states a) - 1], [((lookUp a' (zip (states a) ([0..length (states a) - 1]))),gg, (lookUp c (zip (states a) ([0..length (states a) - 1]))))| (a',gg,c) <- trans a])
       
         

lookUp :: Eq q =>  q -> [(q,Int)] ->  Int
lookUp q' qis =  the [ i | (q,i) <- qis, q == q' ]
  where
  the [q] = q

stringFSM :: String -> FSM Int
stringFSM xs =  ([0..n], ['a'..'z'] ,[0], [n], [(a, g, a+1) | (a,g) <- zip [0..n-1] xs])
  where n = length xs

prop_stringFSM m n =
  accepts a s
  && accepts a t == (s == t)
  where
  a = stringFSM s
  s = safeString m
  t = safeString n

--14.

completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM m = (map Just (states m), alph m, map Just (start m), map Just (final m), [if (a,b) `elem` allPoss then (Just a,b,Just c) else (Just a,b,Nothing)| (a,b,c) <- trans m]) 
                where allPoss = [(a,b) | a <- states m, b <- alph m1]

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Maybe q, Maybe q')
unionFSM = undefined
        
prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  accepts fsm u == (accepts a u || accepts b u)
  && accepts fsm s
  && accepts fsm t
  where
  fsm = unionFSM a b
  a = stringFSM s
  b = stringFSM t
  c = stringFSM u
  s = safeString m
  t = safeString n
  u = safeString o

--15.

star :: (Ord q) => FSM q -> FSM q
star = undefined

prop_star :: String -> Int -> Bool
prop_star m n =
  accepts fsm (concat (replicate i s))
  where
  fsm = star (stringFSM s)
  s = safeString m
  i = abs n

--16.

complementFSM :: (Ord q) => FSM q -> FSM (Maybe q)
complementFSM = undefined
           
prop_complement :: String -> String -> Bool
prop_complement m n =
  not (accepts fsm s)
  && accepts fsm t == not (s == t)
  where
  fsm = complementFSM (stringFSM s)
  s = safeString m
  t = safeString n

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
intersectFSM = undefined
                
prop_intersectFSM1 m n =
  accepts fsm s
  && accepts fsm t == (s == t)
  where
  fsm = intersectFSM a a
  a = stringFSM s
  s = safeString m
  t = safeString n

prop_intersectFSM2 m n o =
  accepts fsm u == (accepts a u && accepts b u)
  where
  fsm = intersectFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

prop_intersectFSM3 m n o =
  accepts fsm s
  && accepts fsm u == accepts a u
  where
  fsm = intersectFSM a (unionFSM a b)
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o
