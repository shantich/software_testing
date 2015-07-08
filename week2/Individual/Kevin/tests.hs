{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Tests where

{-
	Before trying to run this make sure QuickCheck is up to date (2.7.6 while writing this).
	Update with 'cabal install quickcheck'. Load this file in GHCi and run main.
-}

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import Lab2

-- run main to start all tests
return []
main = $quickCheckAll