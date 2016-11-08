-- For future self:
-- This is a summary of Learn you a Haskell for a great good.
-- You went through every chapter and wrote down the most important stuff.
-- If you encounter "-- >" it means you can test it out in WinGHci or something.
-- If you encounter "-- =" it means the result of the "-- >" above.
-- [Some kind of title] means a new paragraph or chapter.
-- Each chapter can be uncommeneted seperately so that the examples do interfere with eachother.
-- Search for [REREAD] every time you open the file. This are parts that you didn't really understand yet. 
-- Have fun

-- This is not my code. It's all from http://learnyouahaskell.com/ and the paperback version. This is just me learning Haskell by 
-- trying out every example and writing down things I later on want to be able to refer back to in one file.


-- Dictionary
-- Statement : defines an action, consists of expressions (What code does) Ex. (print y)
-- Expression : A collection of symbols that jointly express a quantity. It evaluates to a value. (What code is) Ex. (y = x + 1)
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
-- This function also works with lists that contain multiple values.
-- > replicate 3 10
-- = [10,10,10]

-- [I'm a list comprehension]
-- List comprehensions are a way to filter, transform, and combine lists. 
-- > [x*2 | x <- [1..10]]
-- = [2,4,6,8,10,12,14,16,18,20]
-- In this list comprehension we say that we draw our elements from the list [1..10]
-- [x <- [1..10]] means that x takes on the value of each element that is drawn frow [1..10]
-- In other words, we bind each element fom [1..10] to x. The part before the (|) is the output of the list comprehension. [REREAD]

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
-- To use this function with paramaters rewrite it like this:
multiValueListResult' xs ys = [x+y | x <- xs, y <- ys]

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
-- (True, 'a') has a type of (Bool, Char)

-- Some functions that operate on tuples.
-- fst : takes a pair and returns its first component
-- > fst (8, 11)
-- = 8
-- snd : takes a pair and return its second component
-- > snd (8, 11)
-- = 11
-- These functions only work on pairs.

-- Another example
-- zip : takes two lists, then "zips" them together into one list by matching elements into pairs. Again in lexicographical order.
-- > zip [1,2,3,4,5] [5,5,5,5,5]
-- = [(1,5),(2,5),(3,5),(4,5),(5,5)]
-- Zip can take two lists that contain elements of different types
-- > zip [1..5] ["one", "two", "three", "four", "five"]
-- = [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
-- When de lengths of the lists don't match : 
-- > zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]  
-- = [(5,"im"),(3,"a"),(2,"turtle")]

-- [END CHAPTER1]

-- [Chapter 2 : Types]
-- The (::) operator is read as "has type of".
-- Explicit types are always denoted with the first letter in uppercase.

-- When writing your own functions, you can choose to give them an explicit type declaration. 
-- The following shows the type declaration for the removeNonUppercase function
removeNonUppercase' :: [Char] -> [Char]
removeNonUppercase' st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Specifying a function that takes several paramaters (It is actually currying, but that is explained later on):
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- [Common Haskell Types]
-- Int 
-- -- Stands for integer. Used for whole numbers. It is bounded which means it has a minimum and maximum value.
-- Integer
-- -- Same as Int but not bounded.
-- Float
-- -- A real floating-point number with single precision (4 bytes or 32 bits)
-- Double
-- -- A real floating-point number with double the precision.
-- Bool
-- -- A boolean type. Holds the value True or False.
-- Char
-- -- Represents Unicode characters. Denoted with single qoutes.
-- Tuples
-- -- Are types, which depends on their length and the type of its component(s). 
-- -- Can have at max 62 elements.
-- -- You can have an empty tuple denoted as ().

-- [Type variables]
-- Type variables allow functions to operate on values of various types in a type-safe manner.
-- They show up in type declarations as lowercase letters.
-- Functions that use type variables are called polymorphic functions. A bit like generics in C#.

-- > :t fst
-- = fst :: (a, b) -> a
-- fst takes a tuple and returns an elements that is of the same type as its first item. Even though a and b are different
-- types variables, they don't neccessarily need to be different types. It just means that the first item's type and the return value's
-- type will be the same. 

-- [Type Classes 101]
-- A 'type class' is an interface that defines some behaviour. If a type is an instance of a type class, then it supports and implements
-- the behaviour the type class describes. More specifically, a type class specifies a bunch of functions, and when we decide to make a type an
-- instance of a type class, we define what those functions means for that type.
-- In a type class multiple class constraints are seperated by commas inside the parentheses.

-- Example
-- > :t (==)
-- = (==) :: (Eq a) => a -> a -> Bool
-- The equality operator (==) is actually a function. If a function is composed of only special characters, it's considered an
-- infix function by default. To examine its type, pass it to another function, or call it as a prefix function, we need to surround
-- it in parentheses. 
-- Everything before the (==>) symbol is called a class constraint.
-- This function is read as : The equality function takes any two values that are of the same type and return a Bool. The type of those two values
-- must be an instance of the Eq class. 

-- What follows are some common Haskell type classes.
-- [The Eq type class]
-- Eq is used for types that support equality testing.
-- The functions its instances implement are (==) and (/=).
-- This means that if there's an Eq class constraint for a type variable in a function, it uses == or /= somewhere in its definition.
-- When a type implements a fuction, that means it defines what the function does when used with that particular type. [REREAD]
-- > 5 == 5
-- = True

-- [The Ord type class]
-- Ord is a type class for types whose values can be put in some order.
-- Ord covers all the standard comparison functions such as (>), (<), (>=), (<=)
-- The compare function takes two values whose type is an Ord instance and returns an Ordering.
-- Ordering is a type that can be GT, LT, or EQ. (Greater than, lesser than or equal).
-- > "Kaasbroodje" < "Bier"
-- = False
-- > "Kaasbroodje" `compare` "Bier"
-- = GT

-- [The Show type class]
-- Values whose types are instances of the Show type class can be represented as strings. 
-- The most commonly used function that operates on instances of this type class is show. Show prints a given value as a string.
-- > show 3
-- = "3"

-- [The Read type class]
-- Sort of the opposite of the type class Show. The read function takes a string and return a value whose type is
-- an instance of Read:
-- > read "True" || False
-- = True
-- > read "8.2" + 3.8
-- = 12.0

-- Type annotations are a way to explicitly tell Haskell what type of an expression should be. We do this by adding :: to the end
-- of the expression and then specifying a type.
-- > read "5" :: Int
-- = 5
-- > read "5" :: Float
-- = 5.0

-- [The Enum type class]
-- Enum instances are sequentially ordered types-- their values can be enumerated. 
-- Values can be used in list ranges.
-- They also have defined successors (succ) and predecessors (pred).
-- > ['a'..'e']
-- = "abcde"
-- > [3 .. 5]
-- = [3,4,5]
-- > succ 'B'
-- = 'C'

-- [The Bounded type class]
-- Instances of the Bounded type class have an upper bound and lower bound, which can be checked by using the minBound and maxBound functions.
-- > minBound :: Int
-- = -9223372036854775808

-- These functions are interesting because of their type : 
-- (Bounded a) => a
-- In a sense, they are polymorphic constants. 

-- [The Num type class]
-- Num is a numeric type class. Its instances can act like numbers. 
-- > :t 20
-- = (Num t) => t
-- To be an instance of Num, a type must already be in Show and Eq.

-- [The Floating type class]
-- The Floating type class includes the Float and Double types, which are used to store floating-point numbers.

-- [The Integral type class]
-- Integral is another numeric type class. While Num includes all numbers, including real number integers, the Integral class includes
-- only integral (whole) numbers. This type class includes the Int and Integer types.

-- > fromIntegral :: (Integral, Num b) => a -> b
-- Here we can see that fromIntegral takes an integral number and turns in into a more general number.
-- This is handy when you want to work with both integral and floating-point numbers.

-- > length :: [a] -> Int
-- We cannot add a floating-point number to this list filled with integers.
-- To get around this, we can use fromIntegral:
-- > fromIntegral (length([1,2,3,4]) + 3.2)
-- = 7.2

-- [END CHAPTER2]

-- [Chapter 3 : Syntax in functions]
-- [Pattern matching]
-- Pattern matchin is used to specify patterns to which some data should conform and to deconstruct the data according to
-- those patterns. 
-- When defining functions in Haskell, you can create seperate function bodies for different patterns.

-- Example
lucky :: Int -> String
lucky 7 = "Lucky number seven"
lucky x = "Sorry, no luck for you"
-- > lucky 7
-- = "Lucky number seven"
-- When you call lucky, the pattern is checked from top to bottom. The function conforms to the first pattern only if the function
-- is called with 7. In any other case the value "falls through" the patterns and here the second pattern catches that value.

-- Make sure when you define a function with pattern matching that is should always include a catchall pattern. This way the program
-- doesn't crash when it receives unexpected input.
-- The exception that points you out to this fenomenon: Non-exhaustive patterns in function 'fuctionname'

-- [Pattern matching with tuples]
-- Example
-- The following is a function that adds to 2D vectors together. First without pattern matching
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

-- Now with pattern matching
addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + y1, x2 + y2)
-- This function makes clear that it takes tuples as paramaters and it also increases readability by giving names to the type 

-- [Pattern matching with lists and list comprehensions]
-- To use pattern matching in list comprehensions:
-- > let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
-- > [a + b | (a,b) <- xs]
-- = [4,7,6,8,11,4]
-- If the pattern match fails, the list comprehension will just move on to the next element, and the element that failed won't be
-- included in the resulting list.
 
-- Regular lists can also be used in pattern matching. You can match with the empty list [] or any pattern that involves the : and the empty list.
-- A pattern like x:xs will bind the head of the list to x and the rest of it to xs. If the list has only one single element (empty) then 
-- x will be empty.

-- The x:xs pattern is often used with recursive functions. However, patterns that include the : character will match only
-- against lists of length one or more.

-- Example
head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x
-- We must surround the bounding variables in parentheses so Haskell can properly parse them.
-- The error function here results in a runtime error.

-- Example
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
-- In this function (x:[]) and (x:y:[]) could be rewritten as [x] and [x,y]. However, we can't write (x:y:_) using square brackets,
-- because it matches any list of length 2 or more. 

-- Example
-- Writing the length function with pattern matching
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs
-- > length'' "Ham"
-- = 3
-- The way this recursive function works:
-- First: we check if it's and empty list. In this case it isn't so is falls through to the second pattern.
-- This pattern matches, so it says : 1 + length'' "am", because we broke it into a head and a tail and discarded the head.
-- The next call of length is : 1 + length'' "m", so we have 1 + (1 + length'' "m").
-- The next step is 1 + length'' "", or 1 + length'' []. This matches the first case and thus = 0.
-- This results in 1 + (1 + (1 + 0))

-- Example
-- sum
-- We know wthat the sum of an empty list is 0, so we write that down as a pattern. The same counts for the multiplying. 
-- In the case of multiplying the base case is 1. 
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

-- Note: you can't use the (++) operator in pattern matching. If you tried to pattern match (xs ++ ys), Haskell wouldn't
-- be able to tell what would be in the xs list and what would be in the ys list. 

-- [As patterns]
-- As-patterns allow you to break up an item according to a pattern and binding it to names whilst keeping a reference to the original item.
-- To create an as-patterns, precede a regular pattern with a name and an @ character. 
-- Example
firstLetter :: String -> String  
firstLetter "" = "Empty string, whoops!"  
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- > firstLetter "Kaas"
-- = "The first letter of Kaas is K"

-- [Guards]
-- Guards are a bit like if's but way more readable inside a pattern match.

-- Example
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!" 
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!" 

-- The function above modified so that it works with functions inside the guards :
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!" 

-- Note: There is no = after the function name and its paramaters, before the first guard.
-- If there is, you get a syntax error. 

-- Example
-- Our own max function
max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

-- Example
-- Our own compare function (The function returns an ordering because the type class Ord always returns an ordering : LT, GT, EQ)
myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- Note : Not only can we call functions as infix with backticks, we can also define them using backticks. Sometimes it's easier to read that way.

-- [Where]
-- The 'where' keyword lets you store the result of intermediate computations.
-- We could use this mechanism in the previous example

bmiTell'' :: (RealFloat a) => a -> a -> String  
bmiTell'' weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
-- Here 'bmi' is visible to all guards, but not outside the function ofcourse.

-- [Pattern matching with where]
-- Example
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstname
            (l:_) = lastname
-- Here we could have used pattern matching directly in the function's paramaters, but this is just as an example.

-- [Functions in where blocks]
-- Instead of a constant we can also use functions inside the where blocks
-- Example
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
-- In this function we can't calculate the BMI from just the function's paramaters, so we have to introduce the 'bmi' function in the where block.
-- We have to examine the list passed to the function and there's a different BMI for every pair in there.

-- [let keyword]
-- let expressions are similar to where bindings. where allows you to bind to variables at the end of a function, and those variables
-- are visible to the entire function, including all its guards. let expressions, on the other hand, allow you to bind to variables anywhere
-- and are expressions themselves.
-- let's are very local, so they don't span across guards.
-- let's can be used in pattern matching.
-- Example
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea
-- let expressions take the form of let <bindings> in <expression>. The names that you define in the let part are accessible to the expression after the in part.
-- The difference between 'let' and 'where' is that let bindings are expressions themselves. where bindings are just syntactic constructs. If something is an
-- expression, then it has a value. This means you can use let expressions almost anywhere.

-- Examples
-- > 4 * (let a = 9 in a + 1) + 2
-- = 42

-- They can also be used to introduce functions in a local scope:
-- > [let square x = x * x in (square 5, square 3, square 2)]

-- They can be seperated with semicolons, which is helpful when you want to bind several variables inline and can't align them in columns.
-- > (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
-- = (6000000,"Hey there!") 

-- Pattern matching with let expressions can be very useful for quickly dismantling a tuple into components and binding those components to names, like this:
-- > (let (a, b, c) = (1, 2, 3) in a+b+c) * 300
-- = 600
-- In this function we use a let expression with a pattern match to deconstruct the triple (1, 2, 3). We call its first component a, its second component b,
-- and its third component c. The in a+b+c part says that the whole let expression will have the value of a+b+c. Finally, we multiply that value by 100.

-- let expressions can't be used across guards because they are expressions and have fairly local scope. Also 'where' is defined bindings are defined after
-- the function they're being used in. This allows the function body to be closer to its name and type declaration, which makes code more readable.

-- [let in list comprehensions]
-- If we rewrite our previous example with a let expression inside a list comprehension instead of a where:
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
-- Here each time the list comprehension takes a tuple from the original list and binds its components to w and h, the let expression
-- binds w / h ^ 2 to the name bmi. Then we just present bmi as the output of the list comprehension.

-- We include a let inside a list comprehension much as we would use a predicate, but instead of filtering the list,
-- it only binds values to names. The names defined in this let are visible to the output (the part before the |), and everything in this list comprehension
-- that comes after the let. So we could make this function only return less skinny people:
calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 
-- The predicate is added after the let expression.
-- the (w, h) <- xs part of the list comprehension is called the generator.
-- We can't use the bmi name in the (w, h) <- xs part because it's defined prior to the let binding.

-- [let in GHCi]
-- The 'in' part of the binding can also be omitted when defining functions and constants directly in GHCi. If we do that, then the names
-- will be visible throughout the entire interactive session:
-- > let zoot x y z = x * y + z
-- > zoot 3 9 2
-- = 29
-- > let boot x y z = x * y + z in boot 3 4 2
-- = 14
-- > boot
-- = <interactive>:14:1: error: Variable not in scope: boot
-- Because we omitted the 'in' part in our first line, GHCi knows we're only using zoot in that line, so it remembers it for the rest of the session.
-- However, in the second let expression, we included the in part and called boot immediately with some paramaters. A let expression that doesn't leave
-- out the in part is an expression in itself and represents a value, so GHCi just printed the value. 

-- [case expressions]
-- case expressions allow you to execute blocks of code for specific values of a particular variable.
-- [REREAD] Summerize this bit later. Just use pattern matching for now (or forever).

-- [END CHAPTER3]

-- [Chapter 4 : Recursion]
-- A recursive function is a function that calls itself. The strategy of such a function is to break down the problem at hand into
-- smaller problems of the same kind and then try to solve those subproblems, breaking them down further if neccessary.
-- Eventually we reach the base case of the problem, which can't be broken down any more and whose solutions need to be explicitly
-- defined by the programmer.

-- Examples
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs


-- [Chapter 12 : Monoids]

-- List type as applicative functor
-- <*> takes every function out of the list that is its left parameter and applies it to every value in that list that is on the right.
-- This results in every possible combination of applying a function from the left list to a value in the right list:
-- Example:
-- ghci> [(+1), (*100), (*5)] <*> [1,2,3]
-- [2,3,4,100,200,300,5,10,15]

