f :: [Int] -> Int
f xs = maximum [if x < 0 then 0 else x | x <- xs]

frecu :: [Int] -> Int
frecu [] = 0
frecu (x:xs)
    | x > 0 = x `max` frecu xs
    | otherwise = frecu xs


p :: [Int] -> [Int]
p xs | even (length xs)
    = sum [ xs!!(i+1) * xs!!i | i <- [0..length xs-1], even i]

prec :: [Int] -> [Int]
prec [] = 0
p (x:y:zs) = x*y + q za
