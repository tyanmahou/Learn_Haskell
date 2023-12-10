import System.Random
import Control.Monad (when, unless)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR(1, 10) gen :: (Int, StdGen)
    putStrLn "Witch 1 to 10"
    numberString <- getLine
    unless (null numberString) $ do
      let number = read numberString
      if randNumber == number
        then putStrLn "You are correct"
        else putStrLn $ "Sorry, it was" ++ show randNumber
      askForNumber newGen