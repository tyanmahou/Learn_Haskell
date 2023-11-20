import Data.Char
import Data.List
import Data.Map qualified as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode shift = encode (negate shift)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

firstTo40 :: Maybe Int
firstTo40 = firstTo 40

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs =
  foldr
    (\(k, v) acc -> if key == k then Just v else acc)
    Nothing
    xs

phoneBook =
  [ ("betty", "555-2938"),
    ("betty", "556-2938"),
    ("bonnie", "452-2928")
  ]

phoneBookM :: Map.Map String String
phoneBookM = Map.fromList phoneBook

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs