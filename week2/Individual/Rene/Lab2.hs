module Lab2 where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

-------------
-- Exercise 1
-------------
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c |
			a > b + c || b > a + c || c > a + b = NoTriangle |
			a == b && b == c = Equilateral |
			a*a == b*b + c*c || b*b == a*a + c*c || c*c == a*a + b*b = Rectangular |
			a == b || b == c || a == c = Isosceles |
			otherwise = Other


testTriangle :: Bool
testTriangle |
	not (prop_isNoTriangle (1,2,5) True) = error "Positive 'No Triangle' test failed" |
	not (prop_isNoTriangle (1,2,3) False) = error "Negative 'No Triangle' test failed" |
	not prop_isTriangle = error "Is 'Triangle' test failed" |
	not prop_isEquilateral = error "Is 'Equilateral' test failed" |
	not prop_isRectangular = error "Is 'Rectangular' test failed" |
	not prop_isIsosceles = error "Is 'Isosceles' test failed" |
	not prop_isOther = error "Is 'Other' test failed" |
	otherwise = True

prop_isNoTriangle :: (Integer, Integer, Integer) -> Bool -> Bool
prop_isNoTriangle (a,b,c) expected = (triangle a b c == NoTriangle) == expected

prop_isTriangle = triangle 1 2 3 /= NoTriangle
prop_isEquilateral = triangle 3 3 3 == Equilateral
prop_isRectangular = triangle 3 4 5 == Rectangular
prop_isIsosceles = triangle 1 2 2 == Isosceles
prop_isOther = triangle 1 2 3 == Other

-------------
-- Exercise 2
-------------
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys =
			length xs == length ys &&
			(and $ map (\x -> countElement x xs == countElement x ys) xs)
				where countElement z zs = length (filter (==z) zs)


-------------
-- Exercise 3
-------------
prop_permutation :: Eq a => [a] -> [a] -> Bool -> Bool
prop_permutation xs ys expected = (isPermutation xs ys) == expected

pTest1 = prop_permutation [1,2,3] [3,1,2] True
pTest2 = prop_permutation [1,2,3] [1,2,4] False
pTest3 = prop_permutation [1,2,3] [1,2,3,4] False
pTest4 = prop_permutation [1,2,3,4] [1,2,3] False

-------------
-- Exercise 4
-------------
{--
Had to copy the books answer for 6.28 because I couldn't figure it out myself.
I do understand how the solution works.

The amount of possible permutations is equal to the factorial of the amount of elements
in the input list. I can use this knowledge to test if the output list has the required
length.
--}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
	where
	insrt :: a -> [a] -> [[a]]
	insrt x [] = [[x]]
	insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


-------------
-- Exercise 5
-------------
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && checkEquality xs ys
	where
	checkEquality :: Eq a => [a] -> [a] -> Bool
	checkEquality [] [] = True
	checkEquality (x:xs) (y:ys) |
		x == y = False |
		otherwise = checkEquality xs ys


-------------
-- Exercise 6
-------------
deran :: Int -> [[Int]]
deran n = derangements [0..(n-1)]
	where
	derangements :: Eq a => [a] -> [[a]]
	derangements xs = filter (isDerangement xs) $ perms xs