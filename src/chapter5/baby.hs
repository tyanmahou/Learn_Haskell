compareWithHundread :: Int -> Ordering
compareWithHundread = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

multThree:: Int ->Int->Int ->Int
multThree x y z = x * y *z