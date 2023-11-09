removeNonUpperCase :: String -> String
removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial' :: Int -> Int
factorial' n = product [1 .. n]

factorial :: Integer -> Integer
factorial n = product [1 .. n]

factorial'' n = product [1 .. n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

hoge :: a -> a
hoge a = a