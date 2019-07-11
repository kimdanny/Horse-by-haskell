--import Test.Quickcheck

doubleme x = x + x

doubleus x y = doubleme x + doubleme y

doublesmallnumber x =
    (if x > 100
    then x
    else doubleme x)

boombangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
      -- xs here must be a list.
      -- so if you type boombangs 13, you will get an error
      --remember this is an 'list' comprehension

-- NEVER USE 'let' in the code. IT IS ONLY FOR CONSOLE.
nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
puttogether = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

removelower :: String -> String
removelower xs = [c | c <- xs, c `elem` ['A'..'Z']]

-- nested list comprehension
xxs = [
        [1,3,5,2,3,1,2,4,5],
        [1,2,3,4,5,6,7,8,9],
        [1,2,4,2,1,6,3,1,3,2,3,6]
      ]
hello = [ [ x | x <- xs, even x ] | xs <- xxs]

-- Multiple parameters
addthree :: Int -> Int -> Int -> Int
addthree x y z = x + y + z

factorial :: Integer -> Integer
factorial x = product [1..x]

-- Recursive definition of factorial
factorial_Recursive :: (Integral a) => a -> a
factorial_Recursive 0 = 1
factorial_Recursive x = x * factorial_Recursive (x-1)

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Seven!!"
lucky x = "You are unlucky pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe x = "neither one nor two"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++  show x
tell (x:y:[]) = "The list has two elements: " ++  show x ++ " and " ++  show y
tell (x:y:_) = "This list is long. The first two elements are: " ++  show x ++ " and " ++  show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

firstletter :: String -> String
firstletter "" = "empty string!"
-- all@(x:xs) : Here, all is the same as the whole list. Same as (x:xs).
firstletter all@(x:xs) = "first letter of " ++ all ++ " is " ++ [x]


bmitell :: (RealFloat a) => a -> a -> String
bmitell weight height
      | bmi <= skinny = "You're underweight, you emo, you!"
      | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
      | bmi <= fat = "You're fat! Lose some weight, fatty!"
      | otherwise = "You're a whale, congratulations!"
      where bmi = weight/height ^ 2
            -- skinny = 18.5
            -- normal = 25.0
            -- fat = 30.0
            -- this is possible as well
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- by defining funciton in where statement ----->IMPORTANT!
calcbmi :: (RealFloat a) => [(a, a)] -> [a]
calcbmi xs = [bmi w h | (w, h) <- xs]
        where
          bmi w h = calc w h
              where
                calc x y = x/y ^ 2

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
        where
          (f:_) = firstName
          (l:_) = lastName


describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
      where what [] = "empty."
            what [x] = "a singleton list."
            what xs = "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ case xs of [] -> "empty"
                                                [x] -> "a singleton list"
                                                xs -> "a longer list."

--          <recursion>       --

fibonacci :: (Integral a) => a -> a
fibonacci x = case x of 0 -> 0
                        1 -> 1
                        x -> fibonacci (x-1) + fibonacci (x-2)

fb :: (Integral a) => a -> a
fb 0 = 0
fb 1 = 1
fb x = fb (x-1) + fb (x-2)

max' :: (Ord a) => [a] -> a
max' [] = error "empty list!"
max' [x] = x
max' (x:xs)
      | x > maxtail = x
      | otherwise = maxtail
      where maxtail = max' xs

--another version using max function
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--replicate function
-- we have to specify both the Num and Ord class constraints when doing
-- addition or subtraction and also comparison.
copy :: (Num i, Ord i) => i -> a -> [a]
copy n x
      | n <= 0 = []
      | otherwise = x:copy (n-1) x

--take function
take' :: (Num i, Ord i) => i -> [a] -> [a]
-- underscore is a 'dont care' syntax
take' n _
      | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

--reverse function
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--repeat function that returns an infinite list
repeat' :: a -> [a]
repeat' x = x : repeat' x

--zip function
zip' :: [a] -> [b] -> [(a, b)]
zip' _[] = []
zip' []_ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- elem function
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
      let smallerSorted = quicksort [a | a <- xs, a <= x]
          biggerSorted = quicksort [a | a <- xs, a > x]
      in smallerSorted ++ [x] ++ biggerSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' (filter (<=x) xs) ++ [x] ++ quicksort' (filter (>x) xs)


--Define a recursive function that insert one integer in a sorted list in a sorted order
-- insert 3 [1, 2, 4, 5] gives [1, 2, 3, 4]
insert :: Int -> [Int] -> [Int]
insert y [] = [y]
insert y (x:xs) | y < x = y:x:xs
                | otherwise = x : (insert y xs)

insert' :: Int -> [Int] -> [Int]
insert' y [] = [y]
insert' y xs = [a | a <- filter (<=y) xs] ++ [y] ++ [a | a <- filter (>y) xs]

-- --------<higher order function>   ---------
-- definition : a function that can take functions as parameters and
--             return functions as return values.

comparewithhundred :: (Num a, Ord a) => a -> Ordering
comparewithhundred = compare 100

dividebyten :: (Floating a) => a -> a
dividebyten = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyThreeTimes :: (a -> a) -> a -> a
applyThreeTimes f x = f (f (f x))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- find the largest number under 100,000 that's divisible by 3829.
--To do that, we'll just filter a set of possibilities in which we know the solution lies.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter divisible' [100000, 99999..])
                          where
                            divisible' x = x `mod` 3829 == 0

-- If that number is even, we divide it by two.
--If it's odd, we multiply it by 3 and then add 1 to that.
--We take the resulting number and apply the same thing to it, which produces a new number and so on.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
        | odd x = x:chain (x * 3 + 1)
        | even x = x:chain (x `div` 2)
        -- here, you can't do x / 2 becuase type of x/2 is Fractioal not Integral

--for all starting numbers between 1 and 100, how many chains have a length greater than 15?
isLong :: [a] -> Bool
isLong xs | length xs > 15 = True
          | otherwise = False

numLongChain :: Int
numLongChain = length( filter isLong (map chain [1..100]) )

numLongChain' :: Int
numLongChain' = length( filter isLong' (map chain [1..100]) )
                  where
                    isLong' xs = length xs > 15

-- Using Lambda function
-- Instead of using isLong we used (\xs -> length xs > 15)
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

--        <Folding>      --
-- folding takes three parameters.
-- binary funciton, starting value (accumulator), a list to fold up
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\x y -> x + y) 0 xs
-- (\x y -> x + y) is the same as (+)

-- curried
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x==y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

max'' :: (Ord a) => [a] -> a
max'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' xs = foldl (\acc x -> x : acc) [] xs

product' :: (Integral a) => [a] -> a
product' [] = error "empty list!"
product' xs = foldr1 (\acc x -> acc * x) xs
--product' = foldr1 (*)   can also do that

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

head' :: [a] -> a
head' = foldl1 (\acc _ -> acc)

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

-- Function composition
-- composition is associative => f.(g.h) = (f.g).h
toNegative :: (Num a) => [a] -> [a]
toNegative xs = map (negate . abs) xs

-- negation of sum of tail
nst :: (Num a) => [[a]] -> [a]
nst xs = map (negate . sum. tail) xs

-- combi [(8, 2), (3, 6), (7, 0.2)] = [15, 2.2]
-- combi :: (Num a, Ord a) => [(a, a)] -> [a]
-- combi xs =
--            let filtered = [(a, b) | (a, b) <- xs, a > b]
--                adding xs = [a, b | ]
--            in adding filtered
combi :: (Real a) => [(a,a)] -> [a]
combi xs =     [sum[a | x <- xs , let a = fst x, let b = snd x, a > b]]
            ++ [sum[b | x <- xs , let a = fst x, let b = snd x, a > b]]


isPrime :: (Integral a, Eq a) => a -> Bool
isPrime x = not (elem 0 mapped)
            where
              mapped = map (x `mod`) [2..(x-1)]

-----   IO    -----
getTwo :: IO (Char, Char)
getTwo = do x <- getChar
            y <- getChar
            return (x, y)

-- getLine' :: IO String
-- getLine' = do x <- getChar
--               if x=='\n' then
--                 return []
--               else
--                   do xs <- getLine'
--                     return (x:xs)

putStr' :: String -> IO()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO()
putStrLn' xs = do putStr' xs
                  putChar '\n'

stringLength :: IO()
stringLength = do putStr "Enter a String : "
                  xs <- getLine
                  putStr "The String has "
                  putStr (show(length xs))
                  putStrLn " characters"


swapAround = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

----- Quickcheck and property of functions---
pyth :: Integer -> Integer -> Integer
pyth x y = (x^2) + (y^2)

prop_pyth :: Integer -> Integer -> Bool
prop_pyth x y = (pyth x y) == ( (x+y)^2 - 2*x*y )

--

-- data NTree = NilTree | Int (NTree) (NTree)
--
-- ntSum :: NTree -> Int
-- ntSum NilTree = 0
-- ntSum (NTree x l r) = x + ntSum(l) + ntSum(r)
--
-- ntList :: NTree -> [Int]
-- ntList NilTree = []
-- ntList (NTree x l r) = [x] ++ ntList(l) ++ ntList(r)

iter :: Int -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = f (iter (n-1) f x)

iterho :: Int -> (Int -> Int) -> Int -> Int
iterho 0 _ x = x
iterho n f x = (foldr (.) (\x -> x) [f | a <- [1..n]]) x

snoc :: Char -> String -> String
snoc x [y] = [y] ++ [x]

rev :: String -> String
rev [] = []
rev xs = foldr (snoc) [] xs

-- definition of foldr using recrusion
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z [] = z
foldr2 f z (x:xs) = f x (foldr f z xs)


--
