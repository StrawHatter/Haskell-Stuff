-- Informatics 1 - Functional Programming
-- Tutorial 2
-- 
-- Week 4 - due: 10/11 Oct.
import Data.Char
import Data.List


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate i xs 
    | 0 <= i && i <= length xs = drop i xs ++ take i xs 
    | otherwise = error "You have inputed an invalid input" 
    
intate :: Int -> [Char] -> [Char]
intate i xs | 0 <= i && i <= length xs = take i xs ++ drop i xs
    | otherwise = error "You have inputed an invalid input"
    
-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
    where
        l = length str
        m = if l == 0 then 0 else k `mod` l

-- 3.
alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey i = zip alphabet (rotate i alphabet)

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp inp [] = chra
lookUp inp ((key,val):restKey)
    | key == inp = val
    | otherwise = lookUp inp restKey
    
-- 5.
encipher :: Int -> Char -> Char
encipher i xs = lookUp xs (makeKey i)

-- 6.
normalize :: String -> String
normalize [] = []
normalize (s:str)
    | isAlpha s = toUpper s : normalize str
    | isDigit s = s : normalize str
    | otherwise = normalize str

-- 7.
encipherStr :: Int -> String -> String
encipherStr i str = [encipher i s | s <- normalize str]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(b,a) | (a,b) <- xs ]

-- 9.
decipher :: Int -> Char -> Char
decipher i inp = lookUp inp (reverseKey (makeKey i))

decipherStr :: Int -> String -> String
decipherStr i xs = [decipher i x | x <- xs, isUpper x || isDigit x || x == ' ']

-- 10.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str substr = isPrefixOf substr str || contains (tail str) substr

-- 11.
candidates :: String -> [(Int, String)]
candidates str = [(i, decipherStr i str) | i <- [0..25], candidate (decipherStr i str)]
    where
        candidate str = str `contains` "AND" || str `contains` "THE"

-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive = undefined
-- 13.
prop_transpose :: String -> Bool
prop_transpose = undefined
-- 14.
encrypt :: Int -> String -> String
encrypt = undefined
-- 15.
decrypt :: Int -> String -> String
decrypt = undefined
-- Challenge (Optional)
-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined
-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined
