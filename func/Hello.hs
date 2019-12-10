import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let ufirstName = map toUpper firstName
        ulastName = map toUpper lastName
    putStrLn $ "hey " ++ ufirstName ++ " " ++ ulastName ++ ", how are you?"
