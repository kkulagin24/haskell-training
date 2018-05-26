import System.IO
import Data.Char

main = do
    contents <- readFile "shortlines.txt"
    writeFile "shortlines.txt" (map toUpper contents)