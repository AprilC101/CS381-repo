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

-- Exercise 1

type Bag a = [(a, Int)]

eBag :: Bag a
eBag = []

-- (a) Define the function ins that inserts an element into a multiset.
-- Might redo this one.  
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x ((y,z):xs)   | x == y    = (y, z+1):xs
                | otherwise = (y,z):ins x xs

-- (b) Define the function del that removes an element from a multiset.

del :: Eq a => a -> Bag a -> Bag a
del x[] = []
del x ((y,z):xs)    | x == y && z >= 2   = (y, z-1):xs
                    | x == y && z == 1  = xs
                    | otherwise = (y,z):del x xs

-- (c) Define a function bag that takes a list of values and produces a multiset representation.
bag :: Eq a => [a] -> Bag a
bag bs = build bs eBag
    where   build [] nb = nb
            build (x:xs) nb = build xs (ins x nb)

-- (d) Define a function subbag that determines whether or not its first argument bag is contained in the second.
-- Need to fix this.  Isn't outputting correct y value in each tuple.
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] b2 = True
subbag ((x,y):xs) b2    | comp (x,y) b2 == True  = subbag xs b2
                        | otherwise = False
    where   comp x [] = False
            comp x (y:ys)   | x == y = True
                            | otherwise = comp x (ys)


-- (e) Define a function isbag that computes the intersection of two multisets.
-- I need to fix this.  It isn't outputting the correct y value in each tuple.
isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag b1 b2 = inter b1 b2 eBag
    where   inter [] b2 nb = nb
            inter ((x,y):xs) b2 nb  | comp (x,y) b2 == True = inter xs b2 ((x,y):nb)
                                | otherwise = inter xs b2 nb
                where   comp x [] = False
                        comp x (y:ys)   | x == y = True
                                        | otherwise = comp x (ys)

--(f) Define a function size that computes the number of elements contained in a bag.
--size :: Bag a -> Int
