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
-- Representations (I guess?)
type Bag a = [(a, Int)]

-- We need eBag (empty bag) for functions that build up and return a bag
eBag :: Bag a
eBag = []


-- (a) Define the function ins that inserts an element into a multiset.
-- Might redo this one.  
ins :: Eq a => a -> Bag a -> Bag a
-- If we don't find the element we simply add it to the multiset/bag
ins x [] = [(x,1)]
-- We look to see if the element is already in the multiset - if it is, we increment its count by 1
-- We recursively call ins until either we find the element or we don't
ins x ((y,z):xs)  | x == y    = (y, z+1):xs
                  | otherwise = (y,z):ins x xs


-- (b) Define the function del that removes an element from a multiset.
del :: Eq a => a -> Bag a -> Bag a
-- If we reach the end of the multiset/bag without finding the element, we do nothing.
del x[] = []
-- We look through the multiset/bag for the element, and if we find it, we decrement its count by 1
-- If its count isn't greater than 1, we simply remove the entire tuple from the multiset/bag
del x ((y,z):xs)    | x == y && z >= 2  = (y, z-1):xs
                    | x == y && z == 1  = xs
                    | otherwise         = (y,z):del x xs


-- (c) Define a function bag that takes a list of values and produces a multiset representation.
bag :: Eq a => [a] -> Bag a
-- We use where to locally define "build".  This allows us to pass in ebag and the argument passed into bag to what
-- I guess amounts to a locally defind function (build).
-- build takes each element in the list of values and uses ins to build up a new multiset/bag
bag bs = build bs eBag
    where   build [] nb = nb
            build (x:xs) nb = build xs (ins x nb)


-- (d) Define a function subbag that determines whether or not its first argument bag is contained in the second.
subbag :: Eq a => Bag a -> Bag a -> Bool
-- If we iterate through all of the tuples in the first bag, then that means bag 1 is contained in bag 2
subbag [] b2 = True
-- We take each tuple in bag 1 and use comp to compare it to each tuple in bag 2.
-- If comp returns true, that means that the tuple from bag 1 that we're currently processing is contained in bag 2
subbag ((x,y):xs) b2    | comp (x,y) b2 == True = subbag xs b2
                        | otherwise             = False
    where   comp (x,y) [] = False
            comp (x,y) ((t,u):ts)       | x == t && y <= u  = True
                                        | otherwise         = comp (x,y) (ts)


--(e) Define a function isbag that computes the intersection of two multisets.
isbag :: Eq a => Bag a -> Bag a -> Bag a
-- We need to populate a new bag with the elements that are found in the intersection of Bags 1 and 2.
isbag b1 b2 = inter b1 b2 eBag
-- Once we've gone through all of the elements in bag 1, we've finished building our new bag/multiset
    where   inter [] b2 nb = nb
            inter ((x,y):xs) b2 nb      | comp (x,y) b2 /= -1 = inter xs b2 ((x, comp (x,y) b2):nb)
                                        | otherwise           = inter xs b2 nb
                where   comp (x,y) [] = -1
                        comp (x,y) ((t,u):ts)   | x == t    = min y u
                                                | otherwise = comp (x,y) (ts)


--(f) Define a function size that computes the number of elements contained in a bag.
-- This one was so easy that I'm worried that I interpreted the instructions incorrectly.
size :: Bag a -> Int
size xs =  sum [y | (x,y) <- xs]  
