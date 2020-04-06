import Data.List (nub,sort)
norm :: Ord a => [a] -> [a]
norm = sort . nub

{-
Homework 1

Group Members: April Child, 

Exercise 1 collaberators:  

Exercise 2 collaberators:  
Exercise 3 collaberators: 
-}

-- Representations, I guess?
type Bag a = [(a, Int)]

eBag :: Bag a
eBag = []

-- (a) inserting element into a multiset
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x ((y,z):xs)   | x == y    = (y, z+1):xs
                | otherwise = (y,z):ins x xs

-- (b) deleting element from a multiset
del :: Eq a => a -> Bag a -> Bag a
del x[] = []
del x ((y,z):xs)    | x == y && z >= 2   = (y, z-1):xs
                    | x == y && z == 1  = xs
                    | otherwise = (y,z):del x xs

-- (c) Converting list into a multiset
bag :: Eq a => [a] -> Bag a
bag bs = build bs eBag
    where   build [] nb = nb
            build (x:xs) nb = build xs (ins x nb)
