module Lab5

where
import Data.List
import Test.Hspec
import Week5

---------------------------
-- Exercise 1
---------------------------

-- !!!!!! PART 1 !!!!!!!
-- Use the same randomly generated Sudoku for all Tests!
testSudoku :: IO ()
testSudoku = do
	rs <- genRandomSudoku
	testSudoku' rs

-- Checking for 3 properties:
-- 	1. Consistency
--  2. Completely filled out
--  3. No constraints left 
testSudoku' :: Node -> IO ()
testSudoku' n = hspec $ do
	describe "Week5.hs" $ do
		it "checks for unique numbers in rows, columns and blocks" $ do
			consistent (fst n) `shouldBe` True

		it "checks for no open positions" $ do
			length (openPositions (fst n)) `shouldBe` 0

		it "checks for no constraints left" $ do
			length (snd n) `shouldBe` 0

-- !!!!!! PART 2 !!!!!!!

-- It is not possible to use QuickCheck for this, because it would try to 
-- generate Random numbers on each position of the grid, which would result 
-- in many inconsistent Sudokus!  



---------------------------
-- Exercise 2
---------------------------

-- A Sudoku Problem is minimal, if it holds for every filled value, 
-- that if you remove it, the solution is not unique anymore.

testProblem :: IO ()
testProblem = do
	rs <- genRandomSudoku
	rp <- genProblem rs
	testProblem' rp

testProblem' :: Node -> IO ()
testProblem' n = hspec $ do
	describe "Week5.hs" $ do
		it "checks if a Problem is minimal" $ do
			showSudoku (fst n)
			and (map (\r -> unremovable n r) (filledPositions (fst n))) `shouldBe` False
			

unremovable :: Node -> (Row,Column) -> Bool
unremovable n x = uniqueSol (eraseN n x)



---------------------------
-- Exercise 3
---------------------------

-- Generate a three-block-free Sudoku by applying the following stepts:
-- 1. Generate all Blocks in the Form [(Row,Column)].
-- 2. Try to minimalize Block by Block and check if the whole Block got empty and if it is still a unique Solution.
-- 3. Minimalize the remaining Sudoku to get a minimal Problem.

genBlockFreeSudoku :: IO ()
genBlockFreeSudoku = do
	rs <- genRandomSudoku
	showSudoku (fst (genBlockFreeSudoku' rs genBlocks))

genBlockFreeSudoku' :: Node -> [[(Row,Column)]] -> Node
genBlockFreeSudoku' n [] = minimalizeRemaining n
genBlockFreeSudoku' n (x:xs) = 
	if (blockEmpty (fst p) (head x)) && uniqueSol p
		then genBlockFreeSudoku' p xs
		else genBlockFreeSudoku' n xs
	where p = removeBlock n x

blockEmpty :: Sudoku -> (Row,Column) -> Bool
blockEmpty s x = length (freeInSubgrid s x) == 9

removeBlock :: Node -> [(Row,Column)] -> Node
removeBlock n xs = minimalize n xs

minimalizeRemaining :: Node -> Node
minimalizeRemaining n = minimalize n (filledPositions (fst n))

genBlocks :: [[(Row,Column)]]
genBlocks = [[ (r',c') | r' <- r, c' <- c] | r <- blocks, c <- blocks]

-- The Solution tries to remove as many Blocks as possible!
-- After some testing, it removes at least 3, somtimes 4.
-- Therefore i come to the conclusion, that removing 4 is possible for some cases, but 5 is impossible,
-- since there are two many variables.



---------------------------
-- Exercise 4 & 5
---------------------------

-- see SudokuNRC.hs

-- use 'solveAndShow example1' to show the solution for the given Sudoku
-- use 'main' to generate a Random NRC Problem

