import Data.Char

countPositives :: [Int] -> Int
countPositives xs = length $ filter (>0) xs

-- countPositives' :: [Int] -> Int
-- countPositives' xs = length $ map isPositive xs
--                     where
--                       isPositive x = x > 0

countPositives'' :: [Int] -> Int
countPositives'' [] = 0
countPositives'' (x:xs)
                      | x > 0 = 1 + (countPositives'' xs)
                      | otherwise = countPositives'' xs

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
                where
                  insert x [] = [x]
                  insert x (y:ys) = if x<=y then x:y:ys else y : (insert x ys)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x<=y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

mysort :: (Ord a) => [a] -> [a]
mysort all@(x:xs) | (length all) <= 1 = all
                 | otherwise = merge leftHalf rightHalf
                                 where
                                   leftHalf = isort [a | a <- xs, a<=x]
                                   rightHalf = isort [a | a <- xs, a>x]













--
