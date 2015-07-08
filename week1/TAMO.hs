module TAMO

where

(&&) :: Bool -> Bool -> Bool
False && x = False
True && x = x

(||) :: Bool -> Bool -> Bool
False || x = x
True || x = True

(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> x = True

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infixr 2 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

p = True
q = False
formula1 = (not p) TAMO.&& (p ==> q) <=> not (q TAMO.&& (not p))
formula2 p q = ((not p) TAMO.&& (p ==> q) <=> not (q TAMO.&& (not p)))

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) TAMO.&& (bf False)

excluded_middle :: Bool -> Bool
excluded_middle p = p TAMO.|| not p

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True <=> bf2 True) TAMO.&& (bf1 False <=> bf2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [(bf1 p q) <=> (bf2 p q) | p <- [True,False], q <- [True,False]]

test1a = not True <=> False
test1b = not False <=> True
test2  = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
test3a = logEquiv1 (\ p -> p TAMO.|| True) (const True)
test3b = logEquiv1 (\ p -> p TAMO.&& False) (const False)
test4a = logEquiv1 (\ p -> p TAMO.|| False) id
test4b = logEquiv1 (\ p -> p TAMO.&& True) id
test5  = logEquiv1 (\ p -> p TAMO.|| not p) (const True)
test6  = logEquiv1 (\ p -> p TAMO.&& not p) (const False)

contrad1 :: (Bool -> Bool) -> Bool
contrad1 bf = not (bf True) TAMO.&& not (bf False)

contrad2 :: (Bool -> Bool -> Bool) -> Bool
contrad2 bf = and [not (bf p q) | p <- [True,False], q <- [True,False]]

contrad3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contrad3 bf = and [not (bf p q r) | p <- [True,False], q <- [True,False], r <- [True,False]]

unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

parity :: [Bool] -> Bool
parity [] = True
parity (x:xs) = x /= (parity xs)

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p