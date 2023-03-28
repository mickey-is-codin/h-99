module Lists where

-- Problem 1 --
-- find the last element of a list
-- no resources
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

-- second solution
-- const always returns first argument passed
-- Prelude> const 1 2
-- 1
-- Prelude> (flip const) 1 2
-- 2
myLast' :: [a] -> a
myLast' = foldr1 (flip const)

-- third solution
-- reverse :: [a] -> [a]
myLast'' :: [a] -> a
myLast'' = head . reverse

-- fourth solution
myLast''' :: [a] -> a
myLast''' x = x !! (length x - 1)

-- Problem 2 --
-- find the last but one element of a list
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [_] = Nothing
myButLast [x, _] = Just x
myButLast (_:xs) = myButLast xs

-- Problem 3 --
-- find the kth element of a list, 1-indexed at
elementAt :: [a] -> Int -> a
elementAt xs ix = xs !! (ix - 1)

-- Problem 4 --
-- find the number of elements in a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- second solution
myLength' :: [a] -> Int
myLength' = foldr (\_ n -> n + 1) 0

-- third solution
myLength'' :: [a] -> Int
myLength'' = foldr (\_ -> (+1)) 0

-- fourth solution
-- const (+1) 2 8 ---> 9
-- so our accumulator always adds to the second value
myLength''' :: [a] -> Int
myLength''' = foldr (const (+1)) 0

-- Problem 5
-- reverse a list
