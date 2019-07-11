import Data.Char

square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth x y = (square x) + (square y)

isTriple :: Int -> Int -> Int -> Bool
isTriple x y z = (pyth x y) == square z

-- isTripleAny :: Int -> Int -> Int -> Bool
-- isTripleAny x y z | maxnum == x = isTriple y z x
--                   | maxnum == y = isTriple z x y
--                   | maxnum == z = isTriple x y z
--                     where
--                       maxnum = max x y z

-- isTripleAny :: Int -> Int -> Int -> Bool
-- isTripleAny x y z == (isTriple x y z) || (isTriple x z y) || (isTriple z y x)

countPositives :: [Int] -> Int
countPositives xs = length [a | a <- xs, a > 0]

capitalised :: String -> String
capitalised [] =[]
capitalised (x:xs) = toUpper x : [toLower x | x <- xs]

halveEvens :: [Int] -> [Int]
halveEvens xs = [if a `mod` 2 == 0 then a `div` 2 else a | a <- xs]
