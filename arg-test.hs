import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "Аргументы командной строки"
    mapM putStrLn args
    putStrLn "Имя программы"
    putStrLn progName