module Lab2 where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

-- ex1
-- Time spent: roughly 40 minutes

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a > b + c || b > a + c || c > a + b = NoTriangle
			   | a == b && b == c 					 = Equilateral
			   | a*a == b*b + c*c || b*b == a*a + c*c || c*c == a*a + b*b = Rectangular
			   | a == b || b == c || a == c 		 = Isosceles
			   | otherwise 							 = Other



-- ex2

isPermutation:: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs) == (length ys) && (and $ map (\x -> count x xs == count x ys) xs)
	where count x = length . filter (==x)

-- ex3

-- In order to test this we should first handle the basis cases which are:
--	xs = [] and xy = [],
--	xs = [] and ys /= [],
--	xs /= [] and ys = [].
-- After that the following cases remain: unequal length (which is always False), duplicates (even though we may assume
-- the input does not contain them, the function processes them correctly), the rest (normal permutations).

testPermutations :: Bool
testPermutations = (isPermutation ([] :: [Int]) ([] :: [Int])) && -- base case 1
				   (not $ isPermutation [] [1]) && -- base case 2
				   (not $ isPermutation [1] []) && -- base case 3
				   (not $ isPermutation [1] [1,2]) && -- Unequal length
				   (isPermutation [1,2,2,3] [2,1,3,2]) && -- Duplicates
				   (isPermutation [1,2,3,4] [4,3,2,1]) && -- Regular True
				   (not $ isPermutation [1,2,3,4] [1,2,3,5]) -- Regular False

-- What does the absence of duplicates mean for your testing procdure?

-- The danger of duplicates is that when creating permutations you have to remove the duplicate permutations.
-- For example, a permutation of [1,2,2,3] is [1,3,2,2]. The problem, however, is that it is being generated twice.

-- ex4

perms :: [a] -> [[a]]
perms [] 	 = [[]]
perms (x:xs) = concat $ map (insert x) $ perms xs
	where
		insert x [] 	= [[x]]
		insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)

-- TODO

-- ex 5

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = (isPermutation xs ys) && (unmoved xs ys)

unmoved :: Eq a => [a] -> [a] -> Bool
unmoved xs ys = and $ zipWith (/=) xs ys

-- ex6
deran :: Int -> [[Int]]
deran n = derangements [0..(n - 1)]

derangements :: Eq a => [a] -> [[a]]
derangements xs = filter (unmoved xs) $ perms xs