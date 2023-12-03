import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "chapter9/todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your To-DO items"
    mapM_ putStrLn numberedTasks
    putStrLn "Witch one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "chapter9/todo.txt"
    renameFile tempName "chapter9/todo.txt"

