import Text.Read (Lexeme(String))
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

third :: (a, b, c) -> c
third (_, _, c) = c

head' :: [a] -> a
head' [] = error "reason"
head' (x : _) = x

firstLetter :: String -> String
firstLetter "" = "empty"
firstLetter all@(x : xs) = "The first letter of" ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "weight"
  | otherwise = "whale"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | bmi <= skinny = "underweight"
  | bmi <= normal = "normal"
  | bmi <= fat = "weight"
  | otherwise = "whale"  
  where bmi = weight / height^2
        skinny = 18.5
        normal = 25.0
        fat    = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
 | a <= b = b
 | otherwise = a

compare' :: Ord a => a -> a-> Ordering
a `compare'` b
 | a == b    = EQ
 | a <= b    = LT
 | otherwise = GT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi w h = w * h^2

calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h^2]

cylinder :: Double->Double->Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = r^2 * pi
    in  sideArea + 2 * topArea

head'2 :: [a] -> a
head'2 xs = case xs of [] -> error "reason"
                       (x:_)->x

describeList :: [a]->String
describeList ls = "The list is " ++ case ls of []-> "empty ."
                                               [x]->"a singleton list"
                                               xs -> "a longer list"

describeList2 :: [a]->String
describeList2 ls = "The list is " ++ what ls
  where what [] = "empty"
        what [x] = "a singleton list"
        what xs = "a longer list"
