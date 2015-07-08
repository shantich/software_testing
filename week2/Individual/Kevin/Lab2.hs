module Lab2 where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

----------------------------------------------
----------------- Exercise 1 -----------------
----------------------------------------------

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a > b + c || b > a + c || c > a + b = NoTriangle
			   | a == b && b == c 					 = Equilateral
			   | a*a == b*b + c*c || b*b == a*a + c*c || c*c == a*a + b*b = Rectangular
			   | a == b || b == c || a == c 		 = Isosceles
			   | otherwise 							 = Other

-- Indicate how you tested or checked the correctness of the program:

-- Time spent:
--



----------------------------------------------
----------------- Exercise 2 -----------------
----------------------------------------------

isPermutation:: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs) == (length ys) && (and $ map (\x -> count x xs == count x ys) xs)
	where count x = length . filter (==x)

----------------------------------------------
----------------- Exercise 3 -----------------
----------------------------------------------

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

----------------------------------------------
----------------- Exercise 4 -----------------
----------------------------------------------

-- For an empty list, the result is a list with an empty list. [[]]
-- For a singleton the result is a list with the singleton in there. [[a]]
-- Then for others you get that a single element is being positioned between the others, this
-- is repeated for all different lists. For example, for [1,2,3,4] and 1 as the changing element
-- you get [1,2,3,4], [2,1,3,4], [2,3,1,4] and [2,3,4,1]. The placing of a single element is done by
-- the `insert x xs` function. This function is mapped over the list recursively, mapping on a smaller list each time.
perms :: [a] -> [[a]]
perms [] 	 = [[]]
perms (x:xs) = concat $ map (insert x) $ perms xs
	where
		insert x [] 	= [[x]]
		insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)

-- What do you know about the number of permutations of a list without duplicates?

-- Amount of items in the current input list n multiplied by the amount of permutations of n-1
-- This is just the factorial of n, where n is the current length of the input list.
-- This results in:
results :: [a] -> Int
results xs = product [1..(length xs)]

-- How do you use this knowledge to test your implementation?
-- The length of the resulting list should be equal to the factorial of the length of the input list.

-- Run the following to test this for lists with a maximum of 5 elements (it slows down rapidly due to it generating all permutations
-- for each list)
prop_results :: Bool
prop_results = and $ map (\xs -> (length $ perms xs) == (results xs)) [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

----------------------------------------------
----------------- Exercise 5 -----------------
----------------------------------------------

-- The derangement of a list is simply the list's permutations minus all lists in which one or more of the elements has not moved.
-- More accurately, a derangement is a permutation where none of the elements is in its original location.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = (isPermutation xs ys) && (unmoved xs ys)

-- Checks whether one of the elements hasn't moved.
-- xs is the input, ys is one of the permutations
unmoved :: Eq a => [a] -> [a] -> Bool
unmoved xs ys = and $ zipWith (/=) xs ys

----------------------------------------------
----------------- Exercise 6 -----------------
----------------------------------------------
deran :: Int -> [[Int]]
deran n = derangements [0..(n - 1)]

-- Gets all derangements of a certain list
derangements :: Eq a => [a] -> [[a]]
derangements xs = filter (unmoved xs) $ perms xs

----------------------------------------------
----------------- Exercise 7 -----------------
----------------------------------------------
-- We'll first have to look at the base cases again:
--	xs = [] and ys = []
--	xs = [] and ys /= []
--	xs /= [] and ys = []

-- After that the following cases remain:
-- unequal length (which is always False),
-- duplicates (where all elements have moved),
-- duplicates (where some elements have not moved),
-- the rest (normal permutations and derangements).

testDerangements :: Bool
testDerangements = (isDerangement ([] :: [Int]) ([] :: [Int])) && -- base case 1
				   (not $ isDerangement [] [1]) && -- base case 2
				   (not $ isDerangement [1] []) && -- base case 3
				   (not $ isDerangement [1] [1,2]) && -- Unequal length
				   (isDerangement [1,2,2,3] [2,1,3,2]) && -- Duplicates (all moved)
				   (not $ isDerangement [1,2,2,3] [2,1,2,3]) && -- Duplicates (not all moved)
				   (isDerangement [1,2,3,4] [4,3,2,1]) && -- Regular True (all moved)
				   (not $ isDerangement [1,2,3,4] [1,4,3,2]) && -- Regular False (not all moved)
				   (not $ isDerangement [1,2,3,4] [1,2,3,5]) -- Regular False (no permutation)

----------------------------------------------
----------------- Exercise 8 -----------------
----------------------------------------------
-- Input xs (List of people), create all derangements, pick a random one

sinterklaas :: Eq a => [a] -> IO [a]
sinterklaas xs = do
					let der = derangements xs
					rand <- getStdRandom (randomR (0, (length der) - 1))
					return (der !! rand)

-- Does it follow from your previous tests that your function performs as expected?

-- In the situation where the input list contains a single element the function returns an error.
-- The output of derangements [1] is simply []. The length of that is 0, random then returns an integer
-- between 0 and -1, which causes an index error. Somehow, though, this is expected, cause you simply cannot
-- pick a person when you're the only person and you're not allowed to pick yourself.

----------------------------------------------
----------------- Exercise 9 -----------------
----------------------------------------------
-- According to Wikipedia the recursive function to calculate the number of derangements is:
-- !n = (n-1) * (!(n-1) + !(n-2))
-- There are also a few base cases. (0 -> 1, 1 -> 0, 2 -> 1)

subfactorial :: Int -> Int
subfactorial 0 = 1
subfactorial 1 = 0
subfactorial 2 = 1
subfactorial n = (n-1) * (subfactorial (n-1) + subfactorial (n-2))

prop_subfactorial :: Int -> Bool
prop_subfactorial n = (map subfactorial [0..n]) == ([length $ deran k | k <- [0..n]])