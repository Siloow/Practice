-- Chapter 12 : Monoids

-- List type as applicative functor
-- <*> takes every function out of the list that is its left parameter and applies it to every value in that list that is on the right.
-- This results in every possible combination of applying a function from the left list to a value in the right list:
-- Example:
-- ghci> [(+1), (*100), (*5)] <*> [1,2,3]
-- [2,3,4,100,200,300,5,10,15]

