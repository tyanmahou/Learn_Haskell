import Data.Char
import Control.Monad
import Text.XHtml (color)

main = do 
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " "
                     ++ bigLastName
                     ++ ", how are you?"    

          

main2 = do
    putStr "Hey, "
    putStr "I'm "
    putStrLn "Andy!"

main3 = do
    putChar 't'
    putChar 'e'
    putChar 'h'

main4 = do
    print True
    print 3                     

main5 = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input    

main6 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs        

main7 = do
    let xs = [1,2,3]
    mapM_ print xs

main8 = forever $ do
    putStr "Give me some input:"
    l <- getLine
    putStrLn $ map toUpper l 


main9 = do
    colors <- forM [1,2,3,4] $ \a -> do
        putStrLn $ "Which color " ++ show a ++ " ?"
        color <- getLine
        return color
    putStrLn "The colors are: "
    mapM_ putStrLn colors          

main9' = do
    colors <- forM [1,2,3,4] $ \a -> do
        putStrLn $ "Which color " ++ show a ++ " ?"
        getLine
    putStrLn "The colors are: "
    mapM_ putStrLn colors              