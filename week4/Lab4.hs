module Lab4 where

import SetOrd
import System.Random
import Test.QuickCheck

---------------------------
-- Exercise 1
---------------------------

-- PART 1 : Creating a Random Set From scratch!

-- Generate random Integers using Random Generator
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- generates a List of Random Integers of length n 
generateList :: Int -> IO [Int]
generateList 0 = return []
generateList n = do
	x <- getRandomInt n
	xs <- generateList (n-1)
	return (x:xs)

-- generates a Set of Random Integers in the Range of 0 to n by using the defined list2set function
getRandomSet :: Int -> IO (Set Int)
getRandomSet n = do
	x <- getRandomInt n
	xs <- generateList x
	return (list2set xs)

-- PART 2 : Using QuickCheck to create the random set!

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
	arbitrary = do
		xs <- arbitrary
		return (list2set xs)

prop_set :: (Ord a) => (Set a) -> Bool
prop_set x = (prop_order x) && (prop_unique x)

prop_order :: (Ord a) => (Set a) -> Bool
prop_order x = ordered x
	where 	ordered (Set []) = True
			ordered (Set [a]) = True
			ordered (Set x:y:xs) = x <= y && (ordered (Set xs))

prop_unique :: (Ord a) => (Set a) -> Bool
prop_unique x = 

---------------------------
-- Exercise 8
---------------------------

fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)



