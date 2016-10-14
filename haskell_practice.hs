-- For future self:
-- This is a summary of Learn you a Haskell for a great good.
-- You went through every chapter and wrote down the most important stuff.
-- If you encounter "-- >" it means you can test it out in WinGHci or something.
-- If you encounter "-- =" it means the result of the "-- >" above.
-- [Some kind of title] means a new paragraph or chapter.
-- Each chapter can be uncommeneted seperately so that the examples do interfere with eachother.
-- Have fun


-- Dictionary
-- Statement : defines an action, consists of expressions (What code does)
-- Expression : A collection of symbols that jointly express a quantity. It evaluates to a value. (What code is)
-- 'Let' keyword : Used to define a name (and more..)
-- Predicate : condition

-- [Chapter 1 : Introduction]
-- [Calling functions]

-- (*) is called an infix function.
-- With prefix functions the name comes first, then a space then its paramaters.
-- Function application hsa the highest precedence of all the operations in Haskell.

-- By using backticks, we can call a prefix function as an infix function.
-- > 92 `div` 10
-- This is the same as calling a function like this in an imperative language : bar(bar(3)) 

-- [Baby's first functions]
doubleMe x = x + x
doubleUs x y = x * 2 + y * 2

-- > doubleUs 4 9
-- = 26

-- Functions in Haskell don't have to be defined in any particular order, so it doesn't matter which function comes first in a file.
-- Functions can't begin with a capital letter

-- The following is not a 'function' because it doens't take any paramaters. It is a 'definition' or a 'name'
myNameIsSil = "I love cheese"

-- Functions can also call eachother.
doubleUs' x y = doubleMe x + doubleMe y

-- An if in Haskell is an expression that must return a value, and not a statement.
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

-- The (') denotes either a strict version of a function (i.e. one that isn't lazy), or a slightly modified version.
-- of a function or a variable with a similar name.
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- [An intro to lists]

-- Lists in Haskell are homogeneous data structures, which means they store several elements of the same type.
-- This means we can't have a list of both integers and strings.
lostNumbers = [3,5,3,5,3,2,7567,567567]
-- > lostNumbers
-- = [3,5,3,5,3,2,7567,567567]

-- [Concatenation]
-- The concatenation operator : (++)
-- let concatList = [1,2,4,5] ++ [9,10,11,12]
-- > concatList
-- = [1,2,4,5,9,10,11,12]

-- in Haskell, string are lists of characters. Because of this, we can use list functions on strings.

-- The (++) operator goes through the whole list, so try to avoid using it on big lists.
-- However, adding something to the beginning of a list is a instantaneous operation.
-- This is done with the (:) operator, called the 'cons' operator.
-- The first argument to the cons operator always needs to be a single item of the same type of the list's items.
-- > 5:[1,2,3,4]
-- = [5,1,2,3,4]

-- [1,2,3] is syntactic sugar for 1:2:3:[]

-- [Accessing list elements]
-- The operator : (!!)
-- > [1,2,3,4,5] !! 2
-- = 3

-- [Lists inside lists]
b = [[1,2,3,4],[5,4,3,2,1],[1,2,3]]
-- > b
-- = [[1,2,3,4],[5,4,3,2,1],[1,2,3]]
-- > b !! 2
-- = [1,2,3]

-- [Comparing lists]
-- Lists can be compared if the items they contain can be compared. When compared with boolean operators,
-- they are compared in lexicographical order. This means the two list heads are compared, and if they're equal,
-- the second elements are compared, and so on.
isL1biggerThanL2 = [3,2,1] > [2,1,0]
-- > isL1biggerThanL2
-- = True
isL1biggerThanL2' = [1,2,1] > [2,1,0]
-- > let isL1biggerThanL2'
-- = False

-- [More list operators]
-- head : first element
-- > head [5,4,3,2,1]
-- = 5
-- tail : all but first
-- > tail [5,4,3,2,1]
-- = [4,3,2,1]
-- last : last element
-- > last [5,4,3,2,1]
-- = 1
-- init : all but last
-- > init [5,4,3,2,1]
-- = [5,4,3,2]
-- length
-- > length [1,2,3,4]
-- = 4
-- null : checks if a list is empty
-- > null [1,2,3]
-- = False 
-- reverse
-- > reverse [5,4,3,2,1]
-- = [1,2,3,4,5]
-- take : takes a number and a list and extracts the specified number
-- If we try to take more elements than there are in the list, Haskell just returns the entire list. We if
-- take 0 elements, we get an empty list.
-- > take 3 [5,4,3,2,1]
-- = [5,4,3]
-- drop : similar as take but it drops the specified number from the beginning of a list
-- > drop 4 [5,4,3,6,2,5]
-- = [2,5]

-- [Texas ranges]
-- Ranges are used to make lists composed of elements that can be enumerated.
-- > [1..20]
-- = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- To define a step, separate the first two elements with a comma and specifiy the upper limit
-- > [1,3..6]
-- = [1,3,5]
-- > [3,6..20]
-- = [3,6,9,12,15,18]

-- To make a list in desc order type:
-- > [20, 19..1]

-- You can make infinite lists. A way to use them:
-- > take 24 [13,26..]
-- = [13,26,39,52..321]
-- Because Haskell is lazy, it won't try to evaluate the entire list immediately.
-- Instead, it will wait to see which elements you need to get from the infinite list.

-- A few functions that can be used to produce long or infinite lists
-- cycle : takes a list and replicates its elements indefinitely to form an infinite list.
-- > take 10 (cycle [1,2,3])
-- = [1,2,3,1,2,3,1,2,3]
-- repeat : takes an element and produces an infinite list of just that element. Same as cycle but with one element
-- > take 5 (repeat 5)
-- = [5,5,5,5,5]
-- replicate : is an easier way to create a list composed of a single item. It is like take 5 (repeat 5) with the take part built in.
-- > replicate 3 10
-- = [10,10,10]

-- [I'm a list comprehension]
-- List comprehensions are a way to filter, transform, and combine lists. 
-- > [x*2 | x <- [1..10]]
-- = [2,4,6,8,10,12,14,16,18,20]
-- In this list comprehension we say that we draw our elements from the list [1..10]
-- [x <- [1..10]] means that x takes on the value of each element that is drawn frow [1..10]
-- In other words, we bind each element fom [1..10] to x. The part before the (|) is the output of the list comprehension.

-- It is also possible to add a predicate to the list comprehension
-- > [x*2 | x <- [1..10], x*2 >= 12]
-- = [12,14,16,18,20]

-- Another example that replaces every odd number < 10 with "Kaas" and > 10 with "Bier"
kaasBier xs = [ if x < 10 then "Kaas" else "Bier" | x <- xs, odd x]
-- > kaasBier [7..13]
-- = ["Kaas", "Kaas", "Bier", "Bier"]
-- You can include as many predicates as you want. Seperate them with commas.
-- It is also possible to draw values from multiple lists.
multiValueListResult = [x+y | x <- [1,2,3], y <- [10,100,1000]]
-- > multiValueListResult
-- = [11,101,1001,12,102,1002,13,103,1003]
-- x is draw from [1,2,3] and y is drawn from [10,100,1000]. While x is 1, y takes on every value from [10,100,1000]
-- and adds 1 to every element that y becomes.

-- Another example 
length' xs = sum [1 | _ <- xs]
-- This function will replace every element in a list with 1, and then sums them all up with 'sum' that yield the length of the list.
-- (_) is just a temporary variable to store items as we draw them. We don't care what type the element is.

-- Another example
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
-- This will only include items in a new list that are in ['A'..'Z'] thus removes all lowercase
-- > removeNonUppercase "KAASisBIER"
-- = "KAASBIER"

-- It is also possible to create nested list comprehensions.
listCeption xxs = [ [ x | x <- xs, even x ] | xs <- xxs]
-- > listCeption [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]] 
-- = [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
-- Here the output of the outer list comprehension is another list comprehension. 

-- [Tuples]
-- Tuples are used to store several heterogeneous elements as a single value.
-- Tuples have a fixed size.
-- The size of a tuple is part of its type.
-- > (1, 3)
-- = (1, 3)

-- A tuple of size two (pair) and a tuple of size three (triple) are treated as two distrinct types, which means a list can't be
-- composed of both pairs and triples. A tuple (1, "Kaas") is a different type than ("Kaas", 1).

-- Some functions that operate on tuples.
-- fst : takes a pair and returns its first component
-- > fst (8, 11)
-- = 8
-- snd : takes a pair and return its second component
-- > snd (8, 11)
-- = 11
-- These functions only work on pairs.

-- Another example
-- zip : takes two lists, then "zips" them together into one list by matching elements into pairs. 
-- > zip [1,2,3,4,5] [5,5,5,5,5]
-- = [(1,5),(2,5),(3,5),(4,5),(5,5)]
-- Zip can take two lists that contain elements of different types
-- > zip [1..5] ["one", "two", "three", "four", "five"]
-- = [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
-- When de lengths of the lists don't match : 
-- > zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]  
-- = [(5,"im"),(3,"a"),(2,"turtle")]

-- [Chapter 2 : Types]



-- [Chapter 12 : Monoids]

-- List type as applicative functor
-- <*> takes every function out of the list that is its left parameter and applies it to every value in that list that is on the right.
-- This results in every possible combination of applying a function from the left list to a value in the right list:
-- Example:
-- ghci> [(+1), (*100), (*5)] <*> [1,2,3]
-- [2,3,4,100,200,300,5,10,15]

