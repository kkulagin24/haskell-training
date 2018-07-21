import Data.Char
import Data.List
import Debug.Trace
import Control.Monad

validPhoneNumber :: String -> Bool
validPhoneNumber s = (pattern <$> s) == "(ddd) ddd-dddd"
    where pattern v
            | isDigit v = 'd'
            | otherwise = v


humanReadable :: Int -> String
humanReadable x = intercalate ":" [format (hours x), format (minutes x), format (seconds x)]
    where hours x = div x 3600
          minutes x = div (mod x 3600) 60
          seconds x = mod x 60
          format x
            | x < 10 = '0':show x
            | otherwise = show x

            
            
getPINs :: String -> [String]            
getPINs observed = foldl1 dsa (numbers <$> observed)

numbers '1' = ["1", "2", "4"]
numbers '2' = ["1", "2", "3", "5"]
numbers '3' = ["2", "3", "6"]
numbers '4' = ["1", "4", "5", "7"]
numbers '5' = ["2", "4", "5", "6", "8"]
numbers '6' = ["3", "5", "6", "9"]
numbers '7' = ["4", "7", "8"]
numbers '8' = ["5", "7", "8", "9", "0"]
numbers '9' = ["6", "8", "9"]
numbers '0' = ["0", "8"]


asd x [] = []
asd x (y:ys) = [x ++ y] ++ asd x ys

dsa xs [] = xs
dsa [] _ = []
dsa (x:xs) ys = asd x ys ++ dsa xs ys

factorize :: Integer -> Integer -> [Integer]
factorize _ 1 = [] 
factorize d n 
    | d * d > n = [n]
    | n `mod` d == 0 = d : factorize d (n `div` d)
    | otherwise = factorize (d + 1) n

primeFactors :: Integer -> [Integer]
primeFactors = factorize 2


--concat $ (\xs -> "("++(show.head $ xs)++"**"++(show $ length xs)++")") <$> (group $ primeFactors 17)


prime_factors :: Integer -> String  
prime_factors n = concat.(format <$>).group.(factorize 2) $ n
    where factorize d n 
            | d * d > n = [n]
            | n `mod` d == 0 = d : factorize d (n `div` d)
            | otherwise = factorize (d + 1) n
          format xs
            | length xs == 1 = "("++(show.head $ xs)++")"
            | otherwise = "("++(show.head $ xs)++"**"++(show.length $ xs)++")"

--sum [x * 26 ^ n | n <- [4..0], x <- (\v -> (elemIndex v ['a'..'z'])) <$> "hello"]

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

toDec number alphabet = sum $ zipWith (\a k -> a * (length alphabet) ^ k) (((\v -> hg (elemIndex v alphabet)) <$>).reverse $ number) [0..(length number)]
    where hg Nothing = 0
          hg (Just x) = x
    
fromDec number alphabet
    | number == 0 = [(alphabet !! number)]
    | otherwise = (\d -> alphabet !! d) <$> (reverse $ fromDec' number (length alphabet))
            where fromDec' 0 _ = []
                  fromDec' number base = mod number base : (fromDec' (div number base) base)


--longestSlideDown :: [[Int]] -> Int
--longestSlideDown pyramid = sum.reverse $ findPath pyramid []
--    where findPath [] path = path
--          findPath fullPyramid@(level:pyramid) path--
--                | (length level) == 1 = findPath pyramid ((level !! 0):path)
--                | (maxSumLeftPyramid fullPyramid) > (maxSumRightPyramid fullPyramid) = findPath (init <$> pyramid) ((level !! 0):path)
--                | otherwise = findPath (tail <$> pyramid) ((level !! 1):path)
--          maxSumLeftPyramid pyramid = maxSumPyramid (init <$> pyramid)
--          maxSumRightPyramid pyramid = maxSumPyramid (tail <$> pyramid)
--          maxSumPyramid pyramid = maximum.(sum <$>).sequence $ take 3 pyramid

--                                                                [[75],
--                                                               [95, '64'],
--                                                             [17, 47, '82'],
--                                                           [18, 35, '87', 10],
--                                                         [20, 04, '82', 47, 65],
--                                                       [19, 01, 23, '75', 03, 34],
--                                                     [88, 02, 77, '73', 07, 63, 67],
--                                                   [99, 65, 04, '28', 06, 16, 70, 92],
--                                                 [41, 41, 26, 56, '83', 40, 80, 70, 33],
--                                               [41, 48, 72, 33, 47, '32', 37, 16, 94, 29],
--                                             [53, 71, 44, 65, 25, 43, '91', 52, 97, 51, 14],
--                                           [70, 11, 33, 28, 77, 73, 17, '78', 39, 68, 17, 57],
--                                         [91, 71, 52, 38, 17, 14, 91, 43, '58', 50, 27, 29, 48],
--                                       [63, 66, 04, 68, 89, 53, 67, 30, '73', 16, 69, 87, 40, 31],
--                                     [04, 62, 98, 27, 23, 09, 70, 98, 73, '93', 38, 53, 60, 04, 23]]        
--95, 47, 87, 82, 75, 73, 28, 83, 47, 43, 73, 91, 67, 98
--64, 82, 87, 82, 75, 73, 28, 83, 47, 43, 73, 91, 67, 98
--47, 43, 73, 91, 67, 98
--32, 91, 78, 58, 73, 93
--                                     [75,95,17,35,82,75,7,16,80,16,52,39,58,73,93]
--                              correct[75,64,82,87,82,75,73,28,83,32,91,78,58,73,93]--                                                                       
--                                     [75,64,82,87,82,75,73,28,83,32,91,78,58,73,93]

--longestSlideDown :: [[Int]] -> Int
--longestSlideDown pyramid = sum $ findPath pyramid []
--    where findPath [] path = path
--          findPath ([a]:pyramid) path = findPath pyramid (a:path)
--          findPath fp@(level:pyramid) path
--                | (maxPath (init <$> fp) 0) > (maxPath (tail <$> fp) 0) = findPath (init <$> pyramid) ((level !! 0):path)
--                | otherwise = findPath (tail <$> pyramid) ((level !! 1):path)
--          maxPath [] path = path
--          maxPath ([a]:pyramid) path =  maxPath pyramid (a + path)
--          maxPath ([a,b]:pyramid) path
--                | a > b = maxPath (init <$> pyramid) (a + path)
--                | a == b = max (maxPath (init <$> pyramid) (a + path)) (maxPath (tail <$> pyramid) (b + path))
--                | otherwise = maxPath (tail <$> pyramid) (b + path)


longestSlideDown pyramid = maxSum pyramid 0
    where maxSum [] sum = sum
          maxSum ([a]:pyramid) sum =  maxSum pyramid (a + sum)
          maxSum pyramid sum = max (maxSum (init <$> pyramid) sum) (maxSum (tail <$> pyramid) sum)


longestSlideDown' [] = 0
longestSlideDown' ([a]:pyramid) =  a + longestSlideDown' pyramid
longestSlideDown' pyramid = max (longestSlideDown' (init <$> pyramid)) (longestSlideDown' (tail <$> pyramid))

longestSlideDown33 pyramid = maximum $ foldl sl (head pyramid) (tail pyramid)
    where sl [a] level = zipWith (+) ([a,a]) level
          sl (a:m:acc) (b:level) = (a + b) : (maximum [(x + y) | x <- [a,m], y <- (head level)]) : sl (m:acc) level

example [a] level = zipWith (+) (a:[a]) level
example (a:b:acc) (c:level) = (max (a + c) (b + c)):(example (b:acc) level)