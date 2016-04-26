
each :: Int -> [a] -> [a]
each n xs = [xs !! i | i<- [n-1, n-1+n..length xs - 1]]

skips :: [a] ->[[a]]
skips xs = [each i xs | i <- [1..length xs]]


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > x && y > z = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1, m..1]) ++ "==========\n0123456789\n"
    where c = count xs
          m = maximum c

line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <-xs]

count :: [Integer] -> [Int]
count xs = map(\n -> length $ filter (== n) xs) [0..9]
