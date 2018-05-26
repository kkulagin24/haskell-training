import Data.String
import Data.List
import Data.Char

digpow n p 
    | s `mod` n == 0 = s `div` n
    | otherwise = -1
    where s = sum $ (\(d, p) -> (read [d]::Int) ^ p) <$> (zip (show n) [p..])

evaporator content evap_per_day threshold = (+1) $ length $ takeWhile (\x -> x >= p_threshold) [(1 - p_evap_per_day) ^ p | p <- [1..]]
    where p_evap_per_day = evap_per_day / 100
          p_threshold = threshold / 100

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter dp [a..b]
    where dp = \number -> (==number) $ sum $ zipWith (\d p -> (^p) $ read [d]::Int) (show number) [1..]

--foldl (\(last:acc) bracket -> if elem [last, bracket] ["{}", "[]", "()"] then acc else bracket:last:acc) ([head "[])"]) (tail "[])")

groupCheck :: String -> Bool
groupCheck s = (==0).length $ stack
    where stack = foldl brackets [] s
          brackets [] bracket = [bracket]
          brackets (last:stack) bracket
            | elem [last, bracket] ["{}", "[]", "()"] = stack
            | otherwise = bracket:last:stack


find_shortest :: String -> Integer
find_shortest s = toInteger $ minimum $ length <$> words s

find_shortest' :: String -> Integer
find_shortest' = toInteger . minimum . (length <$>) . words

data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)

arithmetic :: Fractional a => a -> a -> Operation -> a
arithmetic a b Add = a + b
arithmetic a b Divide = a / b
arithmetic a b Multiply = a * b
arithmetic a b Subtract = a - b

sortArray xs = replaceOdd xs (sortedOdd xs) []
    where sortedOdd xs = sort.(filter odd) $ xs
          replaceOdd xs [] acc = acc ++ xs
          replaceOdd (x:xs) all@(y:ys) acc
            | odd x = replaceOdd xs ys (acc ++ [y])
            | otherwise = replaceOdd xs all (acc ++ [x])

zeros :: Int -> Int
zeros n = sum [floor ((fromIntegral n) / (5 ^ k)) | k <- [1..m n]]
    where m n = (floor.(logBase 5).fromIntegral) n


a :: Int-> Int
a b = floor $ logBase 5 (fromIntegral b)

reverseWords :: String -> String
reverseWords = concat.(reverse <$>).(groupBy (\a b -> a /= ' ' && b /= ' '))

rot13 :: String -> String
rot13 s = rr <$> s
    where rr l
            | (not.isAsciiLower.toLower) l = l
            | isUpper l = last.(take 14) $ [l..'Z'] ++ ['A'..l]
            | isLower l = last.(take 14) $ [l..'z'] ++ ['a'..l]

toCamelCase :: String -> String
toCamelCase str = (filter isLetter).reverse.(foldl cc []) $ str
    where cc ('-':acc) l = (toUpper l):acc
          cc ('_':acc) l = (toUpper l):acc
          cc acc l = l:acc

--getPINs :: String -> [String]
getPINs observed = [(++)] <*> (adjacentDigit <$> observed)
    where adjacentDigit '1' = "124"
          adjacentDigit '2' = "1235"
          adjacentDigit '3' = "236"
          adjacentDigit '4' = "1457"
          adjacentDigit '5' = "24568"
          adjacentDigit '6' = "3569"
          adjacentDigit '7' = "478"
          adjacentDigit '8' = "57890"
          adjacentDigit '9' = "689"
          adjacentDigit '0' = "08"

newtype Alphabet = Alphabet { getDigits :: [Char] } deriving (Show)

bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric :: Alphabet
bin = Alphabet $ "01"
oct = Alphabet $ ['0'..'7']
dec = Alphabet $ ['0'..'9']
hex = Alphabet $ ['0'..'9'] ++ ['a'..'f']
alphaLower    = Alphabet $ ['a'..'z']
alphaUpper    = Alphabet $ ['A'..'Z']
alpha         = Alphabet $ ['a'..'z'] ++ ['A'..'Z']
alphaNumeric  = Alphabet $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

convert :: Alphabet -> Alphabet -> String -> String
convert (Alphabet a) (Alphabet b) x = fromDec (toDec x a) b

toDec number alphabet = sum $ zipWith (\a k -> a * (toInteger (length alphabet)) ^ k) (((\v -> toInteger (hg (elemIndex v alphabet))) <$>).reverse $ number) (toInteger <$> [0..(length number)])
    where hg Nothing = 0
          hg (Just x) = x
    
fromDec number alphabet
    | number == 0 = [(alphabet !! (fromIntegral number))]
    | otherwise = (\d -> alphabet !! (fromIntegral d)) <$> (reverse $ fromDec' number (toInteger (length alphabet)))
            where fromDec' 0 _ = []
                  fromDec' number base = mod number base : (fromDec' (div number base) base)

palindromeChainLength :: Integer -> Integer
palindromeChainLength x = toInteger $ searchPalindrome (fromIntegral x) 0
    where searchPalindrome x n
                | isPalindrome x = n
                | otherwise = searchPalindrome (x + (reverseNumber x)) (n + 1)
          reverseNumber n = read.reverse.show $ n::Int
          isPalindrome x = x == (reverseNumber x)
