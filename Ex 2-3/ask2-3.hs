--Λεούσης Σάββας
--Α.M.:03114945

import Test.QuickCheck
import GHC.Real

--Exercise 2

--1

data Tree a = T a [Tree a]
 deriving Show

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T x s) = f x (map (foldTree f) s)

--2

sizeTree :: Num b => Tree a -> b
sizeTree t = foldTree (\x ys -> 1 + sum ys) t

--help function for heighTree
safe_maximum :: (Ord a, Num a) => [a] -> a
safe_maximum [] = 0
safe_maximum xs = maximum xs

heightTree :: (Ord b, Num   b) => Tree a -> b
heightTree t = foldTree (\x ys -> 1 + safe_maximum ys) t

sumTree :: Num a => Tree a ->  a
sumTree t = foldTree (\x ys -> x + sum ys) t

maxTree :: (Ord a) => Tree a -> a
maxTree t = foldTree (\x ys -> maximum ([x] ++ ys)) t

inTree :: Eq a => a -> Tree a -> Bool
inTree x t = foldTree (\y ys -> if (x == y) then True else if or ys then True else False) t

nodes :: Tree a -> [a]
nodes t = foldTree (\x ys -> [x] ++ concat ys) t

countTree :: (a -> Bool) -> Tree a -> Integer
countTree f t = foldTree (\x ys -> if (f x == True) then sum (ys) + 1 else sum ys) t

leaves :: Tree a -> [a]
leaves t = foldTree (\x ys -> if null ys then [x] else concat ys) t

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f t = foldTree (\x ys -> T (f x) ys) t

--3

--help function for trimTree
foldTree_n :: (a -> [b] -> b) -> Int -> Tree a -> b
foldTree_n f n (T x ys) = if n > 0
                            then 
                                (f x (map (foldTree_n f (n-1)) ys))
                            else
                                (f x [])

trimTree :: Int -> Tree a -> Tree a
trimTree n t = foldTree_n (\x ys -> T x ys) n t

path :: [Int] -> Tree a -> a
path [] (T x ys) = x
path (h:t) (T x ys) = path t (ys !! h)


--Exercise 3

--1

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  x <- arbitrary
  n <- choose (0, m `div` 2)
  ys <- vectorOf n (arbitrarySizedTree (m `div` 4))
  return (T x ys)

--2
tree_property_1 :: Tree a -> Bool
tree_property_1 t = (height > 0) && height <= sizeTree t
  where height = heightTree t

tree_property_2 :: (Ord a, Num a) => Tree a -> Bool
tree_property_2 t = inTree (maxTree t) t

tree_property_3 :: Eq a => Tree a -> Bool
tree_property_3 t = and (map (`inTree` t) (nodes t))

tree_property_4 :: (a -> Bool) -> Tree a -> Bool
tree_property_4 f t = (count >= 0) && (count <= sizeTree t)
  where count = countTree f t

tree_property_5 :: Tree a -> Bool
tree_property_5 t = (nodes_count == sizeTree t) && ((leaves_count < nodes_count) || ((leaves_count == nodes_count) && (leaves_count == 1)))
  where nodes_count = length (nodes t)
        leaves_count = length (leaves t)

tree_property_6 :: (a -> b) -> Tree a -> Bool
tree_property_6 f t = (sizeTree t == sizeTree t2) && (heightTree t == heightTree t2)
  where t2 = mapTree f t

tree_property_7 :: (Eq a, Eq b) => (a -> b) -> a -> Tree a -> Bool
tree_property_7 f n t = if inTree n t 
                      then 
                          inTree (f n) (mapTree f t) 
                      else
                          True

tree_property_8 :: (Eq a, Eq b) => (a -> b) -> Tree a -> Bool
tree_property_8 f t = (map f (nod t) == nod (mapTree f t)) && (map f (leaf t) == leaf (mapTree f t))
  where nod = nodes
        leaf = leaves

--3

bird :: Tree Rational
bird = T 1 [mapTree (\x -> 1/(x+1)) bird, mapTree (\x -> 1/x+1) bird]

bird_property_1 :: [Bool] -> Bool
bird_property_1 path_bool =
    let 
        path_int = map (\x -> if x == True then 1 else 0) path_bool
        path_length = length path_bool
    in
        if (path path_int bird) == (path path_int (trimTree (path_length) bird)) then True else False

zig_zag_list :: [Int] -> Int -> [Int]
zig_zag_list l 0 = l
zig_zag_list l n = zig_zag_list ([hd] ++ l) ((abs n)-1)
    where hd = if (abs n) `mod` 2 == 0 then 0 else 1

createZigZagPath :: Int -> [Int]
createZigZagPath n = zig_zag_list [] n

zig_zag_property :: (Num a, Eq a) => [Int] -> a -> Tree a -> Bool
zig_zag_property [] x' t = True
zig_zag_property (h:t) x' (T x ys) = if (x - x' == 1) then zig_zag_property t x (ys !! h) else False

bird_property_2 :: Int -> Bool
bird_property_2 n = zig_zag_property (createZigZagPath n) 0 bird

fast_fib :: Int -> Integer
fast_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = fast_fib (n-2) + fast_fib (n-1)

fib_property :: [Int] -> Tree Rational -> Bool
fib_property [] t = True
fib_property (h:t) (T x ys) = if (denominator x == (fast_fib h)) then fib_property t (ys !! 0) else False

bird_property_3 :: Int -> Bool
bird_property_3 n = fib_property [2..n] bird

getBirdPath :: Rational -> Int -> [Int] -> Tree Rational -> [Int]
getBirdPath q i path (T x ys) = 
    if x == q 
        then path 
    else 
        if i `mod` 2 == 0 then 
            if q < x then 
                getBirdPath q (i+1) (0:path) (ys !! 0) 
            else 
                getBirdPath q (i+1) (1:path) (ys !! 1)
        else 
            if q >= x then
                getBirdPath q (i+1) (0:path) (ys !! 0)
            else 
                getBirdPath q (i+1) (1:path) (ys !! 1)

findBird :: Rational -> [Int]
findBird q = reverse (getBirdPath q 0 [] bird)

bird_property_4 :: Rational -> Bool
bird_property_4 rat = 
        if rat > 0 then
            path (findBird rat) bird == rat 
        else
            True

main = do
  putStrLn "Quickchecking Tree Properties:" 
  putStrLn "Checking property 1..."
  quickCheck (tree_property_1 :: Tree Int -> Bool)
  putStrLn "Checking property 2..."
  quickCheck (tree_property_2 :: Tree Int -> Bool)
  putStrLn "Checking property 3..."
  quickCheck (tree_property_3 :: Tree Int -> Bool)
  putStrLn "Checking property 4..."
  quickCheck (tree_property_4 (\x -> x < 5) :: Tree Int -> Bool)
  putStrLn "Checking property 5..."
  quickCheck (tree_property_5 :: Tree Int -> Bool)
  putStrLn "Checking property 6..."
  quickCheck (tree_property_6 (\x -> x + 17) :: Tree Int -> Bool)
  putStrLn "Checking property 7..."
  quickCheck (tree_property_7 (\x -> x + 17) :: Int -> Tree Int -> Bool)
  putStrLn "Checking property 8..."
  quickCheck (tree_property_8 (\x -> x + 17) :: Tree Int -> Bool)
  putStrLn "Quickchecking Bird Properties:" 
  putStrLn "Checking property 1..."
  quickCheck (bird_property_1 :: [Bool] -> Bool)
  putStrLn "Checking property 2..."
  quickCheck (bird_property_2 :: Int -> Bool)
  putStrLn "Checking property 3..."
  quickCheck (bird_property_3 :: Int -> Bool)
  putStrLn "Checking property 4..."
  quickCheck (bird_property_4 :: Rational -> Bool)