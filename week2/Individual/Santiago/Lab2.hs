module Lab2 where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

-- Exercise 1
-- Time spent: 15 minutes
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | x == y && y == z = Equilateral
			   | x == y || y == z || x == z = Isosceles
			   | x * x == y * y + z * z || y * y == x * x + z * z || z * z == x * x + y * y = Rectangular
			   | x > y + z || y > x + z || z > x + y = NoTriangle
			   | otherwise = Other
			   
-- Exercise 2
-- Time spent: 35 minutes
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation _ [] = error "Second list empty"
isPermutation [] _ = True
isPermutation (x:xs) ys | elem x ys = isPermutation xs ys
	 					| otherwise = False
						
-- Exercise 3
-- See file "test cases.txt"
-- Time spent: --

-- Exercise 4
-- Time spent: 
perms :: [a] -> [[a]]
perms [] 	 = [[]]
perms (x:xs) = concat $ map (insert x) $ perms xs
	where
		insert x [] 	= [[x]]
		insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)

