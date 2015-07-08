module Lab6

where
import Data.List
import System.Random
import Test.QuickCheck
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

------------------------------
-- Exercise 1
------------------------------

-- Basically i used 2 Steps to speed up the function:
-- 1. get a binary representation of the exponent as an array of 1's and 0's
-- 2. decompose the problem

exM' :: Integer -> Integer -> Integer -> Integer
exM' x y m = (decompose (reverse (toBin y)) x m 0) `mod` m

decompose :: [Integer] -> Integer -> Integer -> Integer -> Integer
decompose [] _ _ _ = 1
decompose (x:xs) base modulus 0 
	| x == 1 = (base `mod` modulus) * (decompose xs base modulus (base `mod` modulus))
	| x == 0 = decompose xs base modulus (base `mod` modulus)
decompose (x:xs) base modulus lastN
	| x == 1 = (lastN * lastN `mod` modulus) * (decompose xs base modulus (lastN * lastN `mod` modulus))
	| x == 0 = decompose xs base modulus (lastN * lastN `mod` modulus)

toBin :: Integer -> [Integer]
toBin 0 = []
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]



------------------------------
-- Exercise 2
------------------------------

testEquivalence = quickCheckWith stdArgs { maxSuccess = 5000 } prop_equiv

prop_equiv :: Integer -> Integer -> Integer -> Bool
prop_equiv b e m = if b == 0 || e == 0 || m == 0 
	then True
	else expM (b^2) (e^2) (m^2) == exM' (b^2) (e^2) (m^2)
	

testFastExp = quickCheckWith stdArgs { maxSuccess = 5000 } prop_remGTmod

prop_remGTmod :: Integer -> Integer -> Integer -> Bool
prop_remGTmod b e m = exM' (b^4) (e^4) (m^2) <= (m^2)


testSlowExp = quickCheckWith stdArgs { maxSuccess = 5000 } prop_remGTmod'
	
prop_remGTmod' :: Integer -> Integer -> Integer -> Bool
prop_remGTmod' b e m = if b == 0 || e == 0 || m == 0 
	then True
	else expM (b^4) (e^4) (m^2) <= (m^2)



------------------------------
-- Exercise 3
------------------------------

