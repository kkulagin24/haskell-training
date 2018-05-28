import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception


main = do
    argList <- getArgs
    proceed argList
    
proceed :: [String] -> IO ()
proceed [] = putStrLn "Нет аргументов!"
proceed (command:argList) = dispatch command argList

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "Комманда " ++ command ++ " не определена!"

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "Комманда add принимает в точности два аргумента!"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn "Комманда view принимает в точности один аргумент!"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
remove _ = putStrLn "Комманда remove принимает в точности два аргумента!"