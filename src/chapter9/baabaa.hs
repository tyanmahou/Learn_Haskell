import System.IO
import Data.Char
import Control.Exception

-- main = do
--     handle <- openFile "chapter9/baabaa.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- main = do
--     withFile "chapter9/baabaa.txt" ReadMode $ \handle -> do
--         contents <- hGetContents handle
--         putStr contents

main = do
    contents <- readFile "chapter9/baabaa.txt"
    putStr contents

main2 = do
    contents <- readFile "chapter9/baabaa.txt"
    writeFile "chapter9/baabaacaps.txt" $ map toUpper contents

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode) hClose f

