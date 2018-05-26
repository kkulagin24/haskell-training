main = do
    contents <- getContents
    putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 15) . lines