import System.IO

main = do
    todoItem <- getLine
    appendFile "chapter9/todo.txt" (todoItem ++ "\n")
