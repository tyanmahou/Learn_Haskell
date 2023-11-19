compareWithHundread :: Int -> Ordering
compareWithHundread = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let small = filter (<= x) xs
      large = filter (> x) xs
   in quicksort small ++ [x] ++ quicksort large

largestDivisible :: Integer
largestDivisible = head (filter p [10000, 9999 ..])
  where
    p x = x `mod` 3829 == 0

sumOddSquare :: Int
sumOddSquare = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs >= 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs >= 15) (map chain [1 .. 100]))

listOfFuns :: [Integer -> Integer]
listOfFuns = map (*) [0 ..]

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip' (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

-- aaa :: [a]->[a]
-- aaa = foldl (\acc x -> x:acc) []

-- bbb :: [a]->[a]
-- bbb = foldr (\x acc -> x:acc) []

last' :: [a]->a
last' = foldl1 (\_ x -> x)

head' :: [a]->a
head' = foldr1 (\x _ -> x)

and' ::[Bool]->Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
oddSquareSum =  sum. takeWhile (< 10000) . filter odd $ map (^2) [1..]