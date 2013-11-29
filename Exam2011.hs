f :: [Int] -> Int
f xs = maximum [if x < 0 then 0 else x | x <- xs]

frecu :: [Int] -> Int
frecu [] = 0
frecu (x:xs)
    | x > 0 = x `max` frecu xs
    | otherwise = frecu xs


