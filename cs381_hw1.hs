import Data.List (nub,sort)
norm :: Ord a => [a] -> [a]
norm = sort . nub

{-
Homework 1

Group Members: April Child, Michael Payne

Exercise 1 collaborators:  Michael Payne
Exercise 2 collaborators:  Michael Payne
Exercise 3 collaborators: 
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


-- Exercise 2
type Node = Int
type Edge = (Node,Node)
type Graph = [Edge]
type Path = [Node]

g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

--(a) Define the function nodes :: Graph -> [Node] that computes the list of nodes contained in a given graph.
--For example, nodes g = [1,2,3,4] 
nodes :: Graph -> [Node]
nodes [] = []
nodes ((x,y):xs) = norm (x:y:nodes xs)


--(b) Define the function suc :: Node -> Graph -> [Node] that computes the list of successors for a node in a
--given graph. For example, suc 2 g = [3,4] , suc 4 g = [] , and suc 4 h = [4]
suc :: Node -> Graph -> [Node]
suc n [] = []
suc n ((x,y):xs)        | n == x        = norm(y:suc n xs)
                        | otherwise     = suc n xs


--(c) Define the function detach :: Node -> Graph -> Graph that removes a node together with all of its incident
--edges from a graph. For example, detach 3 g = [(1,2),(2,4)] and detach 2 h = [(1,3),(4,4)]
detach :: Node -> Graph -> Graph
detach n [] = []
detach n ((x,y):xs)     | n == x || n == y      = detach n xs
                        | otherwise             = norm((x,y) : detach n xs)


--(d) Define the function cyc :: Int -> Graph that creates a cycle of any given number. For example, cyc 4 =
--[(1,2),(2,3),(3,4),(4,1)] 
-- This one doesn't make any sense to me.  I guess since we don't have a graph as input, we just build our own cycle?
cyc :: Int -> Graph
cyc n =  norm([(x,x+1) | x <-[1..n], x <= n-1]) ++ [(n,1)]


--Exercise 3
type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
        | Circle Point Length
        | Rect Point Length Length
        deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

 --(a) Define the function width that computes the width of a shape.
width :: Shape -> Length
width (Circle (x,y) r) = r*2
width (Pt _) = 0
width (Rect _ l _) = l

--(b) Define the function bbox that computes the bounding box of a shape.
bbox :: Shape -> BBox
bbox (Circle (x,y) r) = ((x-r,y-r), (x+r,y+r))
bbox (Pt (x,y)) = ((x,y),(x,y))
bbox (Rect (x,y) l1 l2) = ((x,y),(l1+x,l2+y))

--(c) Define the function minX that computes the minimum x coordinate of a shape.
minX :: Shape -> Number
minX (Circle (x,y) r) = x-r
minX (Pt (x,_)) = x
minX (Rect (x,y) _ _) = 3

--(d) Define a function move that moves the position of a shape by a vector given by a point as its second argument.
--I didn't make a helper function like addPt like he suggested, because it didn't seem necessary, which makes me think I'm misinterpreting
-- the problem instructions
move :: Shape -> Point -> Shape
move (Circle (x,y) r) (x1,y1) = (Circle (x+x1,y+y1) r)
move (Pt (x,y)) (x1, y1) = (Pt (x+x1, y+y1))
move (Rect (x,y) l1 l2 ) (x1,y1) = (Rect (x + x1, y + y1) l1 l2)

--(e) Define a function alignLeft that transforms one figure into another one in which all shapes have the same
--minX coordinate but are otherwise unchanged.
alignLeft :: Figure -> Figure
alignLeft f = map (moveToX (minimumX (f))) f
        where   minimumX [f] = minX f
                minimumX (f:fs) = min (minX (f)) (minimumX (fs))

moveToX :: Number -> Shape -> Shape
moveToX n (Circle (x,y) r) = (Circle (x,y) (x-n))
moveToX n (Pt (x,y)) = Pt (n,y)
moveToX n (Rect (x,y) l1 l2 ) = (Rect (n, y) l1 l2)

--(f) Define a function inside that checks whether one shape is inside of another one, that is, whether the area
--covered by the first shape is also covered by the second shape.
inside :: Shape -> Shape -> Bool
inside s (Rect (x,y) l1 l2) = distanceBBox (bbox(s)) <= distanceBBox(bbox(Rect (x,y) l1 l2))
inside (Pt (x,y)) (Circle (x1,y1) r) = round(sqrt(fromIntegral((x1-x)^2 + (y1-y)^2))) <= r
inside (Rect (x,y) l1 l2) (Circle (x1, y1) r) = round(sqrt(fromIntegral(l1^2 + l2^2))) <= r*2
inside s (Pt (x,y)) = False

distanceBBox :: BBox -> Number
distanceBBox ((x,y), (x1,y1)) = round(sqrt(fromIntegral((x1-x)^2 + (y1-y)^2)))
