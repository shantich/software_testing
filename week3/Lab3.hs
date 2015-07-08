module Lab3 where

import Week3

---------------------------
-- Exercise 1
---------------------------

-- If a formula is satisfiable, one of its valuations makes it true. A contradiction is therefore never satisfiable thus negating it gives us the contradiction.
-- used Eta reduction.
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- All Valuations need to evaluate to true
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- Entailment can be rewritten as an Implication, which has to be True for every Valuation
-- Therefore the Implication has to be a tautology
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- Logical Equivalance means, that the truth values for every Valuation is equal. Therefore the equivalent of the two functions has to be a tautology.
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)


---------------------------
-- Exercise 2
---------------------------

-- Transformation into CNF needs 3 basic steps plus an auxiliary step.
--
-- First step: guarantee arrowfreeness
-- Second step: apply negative normal form
-- thir step: apply cunjunctive normal form by using the auxiliary step (DIST)

cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree

-- precondition: arrowfree and nnf
-- postcondition: form is in cnf
cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg (Prop x)) = Neg (Prop x)
cnf' (Cnj fs) = Cnj (map cnf' fs)
cnf' (Dsj []) = Dsj []
cnf' (Dsj fs) = dist (map cnf' fs)

dist :: [Form] -> Form
dist [f] = f
dist (f:fs) = dist' f (dist fs)

dist' :: Form -> Form -> Form
dist' (Cnj f) p = Cnj (map (\ x -> dist' x p) f)
dist' f (Cnj p) = Cnj (map (\ x -> dist' f x) p)
dist' f p = Dsj [f, p]


---------------------------
-- Exercise 3
---------------------------

-- The test has to check for 2 properties:
-- 1. Equivalance of the actual and the transformed formula
-- 2. Correctness according to the Grammar

testCnf :: Int -> IO ()
testCnf 0 = print ("No tests processed! Passed!")
testCnf i = do 
	xs <- getRandomFs i
	testCnf' xs

testCnf' :: [Form] -> IO ()
testCnf' [] = print ("finished!")
testCnf' (x:xs) =
	if equiv x (cnf x) && testGrammar (cnf x)
		then do 
			print (" pass on: " ++ show (cnf x))
			testCnf' xs
	else print ("failed test on: " ++ show x)

-- Grammar:
-- L ::= p | -p
-- D ::= L | L or D
-- C ::= D | D and C

testGrammar :: Form -> Bool
testGrammar (Prop x) = True
testGrammar (Neg (Prop x)) = True
testGrammar (Dsj fs) = and (map isValidDsj fs)
testGrammar (Cnj fs) = and (map isValidCnj fs)

isValidDsj :: Form -> Bool
isValidDsj (Prop x) = True
isValidDsj (Neg (Prop f)) = True
isValidDsj (Dsj p) = and (map testGrammar p)

isValidCnj :: Form -> Bool
isValidCnj (Prop x) = True
isValidCnj (Neg (Prop f)) = True
isValidCnj (Dsj fs) = and (map isValidDsj fs)
isValidCnj (Cnj fs) = and (map testGrammar fs)


---------------------------
-- Exercise 4
---------------------------

type Clause = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls = cnf2cls' . cnf

cnf2cls' :: Form -> Clauses
cnf2cls' (Prop x) = [[x]]
cnf2cls' (Neg (Prop x)) = [[-x]]
cnf2cls' (Dsj fs) = [dsj2cls fs]
cnf2cls' (Cnj fs) = cnj2cls fs

dsj2cls :: [Form] -> Clause
dsj2cls [] = []
dsj2cls (x:xs) = dsj2cls' x ++ dsj2cls xs

dsj2cls' :: Form -> Clause
dsj2cls' (Prop x) = [x]
dsj2cls' (Neg (Prop x)) = [-x]
dsj2cls' (Dsj fs) = dsj2cls fs

cnj2cls :: [Form] -> Clauses
cnj2cls [] = [[]]
cnj2cls (x:xs) = cnf2cls' x ++ cnj2cls xs
