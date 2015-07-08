module TUOLP

where

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
	where
	mark :: [Integer] -> Integer -> Integer -> [Integer]
	mark (y:ys) k m | k == m = 0 : (mark ys 1 m)
					| otherwise = y : (mark ys (k+1) m)

primes :: [Integer]
primes = sieve [2..]

oddsFrom3 :: [Integer]
oddsFrom3 = 3 : map (+2) oddsFrom3

fasterprimes :: [Integer]
fasterprimes = 2 : sieve oddsFrom3

pdivisors :: Integer -> [Integer]
pdivisors n = [ d | d <- [1..(n-1)], rem n d == 0 ]

primePairs :: [(Integer,Integer)]
primePairs = pairs primes
	where
	pairs (x:y:xys) | x + 2 == y = (x,y) : pairs (y:xys)
					| otherwise = pairs(y:xys)
					
primeTriples :: [(Integer,Integer,Integer)]
primeTriples = triples primes
	where
	triples (x:y:z:xyzs)
		| x + 2 == y && y + 2 == z = (x,y,z) : triples (y:z:xyzs)
		| otherwise = triples (y:z:xyzs)