respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPal xs then "palindrome" else "not palindrome") .
    lines

isPal xs = xs == reverse xs

main = interact respondPalindromes