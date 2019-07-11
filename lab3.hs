mult :: (Num a) => [a] -> a
mult = foldl1 (*)

posList :: [Int] -> [Int]
posList = filter (>0)

trueList :: [Bool] -> Bool
trueList xs = foldl (\acc x -> acc&&x) True xs
--trueList = foldl (&&) True

evenList :: [Int] -> Bool
evenList = foldr ((&&) . even) True

maxList :: (Ord a) => [a] -> a
maxList (x:xs) = foldl (max) x xs

-- filter out positive number and then change all number to 1 using map
-- and sum all 1's
countPositives :: [Int] -> Int
countPositives xs =
                  let filtered = filter (>0) xs
                      changedTo1 = map (\x -> 1) filtered
                  in foldl1 (+) changedTo1

length' :: [a] -> Int
length' [] = 0
length' xs = let changed = map (\x -> 1) xs
             in foldl (+) 0 changed

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr ((:).f) [ ] xs
--myMap f xs = foldr (\x acc -> f x : acc) [] xs

myLength' :: [a] -> Int
myLength' xs =  foldr (+) 0 ( map' (\x-> 1) xs)
          where map' f = foldr ((:).f) [ ]











--
