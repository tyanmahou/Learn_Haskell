lucky ::Int ->String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe::Int->String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial::Int->Int
factorial 0 = 1
factorial n = n * factorial (n-1)

charName::Char->String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors::Num a => (a, a)->(a, a)->(a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

third :: (a, b, c) -> c
third (_,_, c) = c

head' :: [a]->a
head' [] = error "reason"
head' (x:_) = x

firstLetter::String->String
firstLetter "" = "empty"
firstLetter all@(x:xs) = "The first letter of" ++ all ++ " is " ++ [x]