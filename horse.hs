newtype Horse = Horse [String]
horse :: Horse
horse = Horse [ "     ,//)     "
              , "     ;;’ \\   "
              , "  ,;;’ ( ’\\  "
              , "      / ’\\_) "]

pretty :: Horse -> IO()
pretty (Horse x) = mapM_ putStrLn x

-- mirroring the horse
mirror :: Horse -> Horse
mirror (Horse x) = Horse (map reverse x)

-- transposing the horse (rotate a horse 90 degrees)
transpose :: [[x]] -> [[x]]
transpose [[]] = []
transpose [[], _] = []
transpose rows = (map head rows) : transpose (map tail rows)

rotate :: Horse -> Horse
rotate (Horse x) = Horse (transpose x)

-- it is possible to perform all possible rotatations using just these two functions

--some sequences
--sequence 1
a :: Int -> Int
a 0 = 1
a n = 2*n + 1

oddnums :: Int -> [Int]
oddnums n = map a [0..n]

-- sequence 2
b :: Int -> Int
b 0 = 0
b n = b (n-1) + n

myseq :: Int -> [Int]
myseq n = map b [0..n]
