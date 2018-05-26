import Data.Char

main = do
    putStrLn "Ваше имя?"
    firstName <- getLine
    putStrLn "Ваша фамилия?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "Привет, " ++ bigFirstName ++ " "
                          ++ bigLastName ++ ""
                          ++ ", как дела??"