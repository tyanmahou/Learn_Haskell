import Data.Monoid
import Data.List
import Control.Monad.Writer
import Control.Monad.State
import Data.Ratio

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog) 

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multiWithLog :: Writer [String] Int
multiWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b);

-- gcd' :: Int -> Int -> Writer [String] Int
-- gcd' a b
--  | b == 0    = do
--     tell ["Finished with " ++ show a]
--     return a
--  | otherwise = do
--     tell  [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
--     gcd' b (a `mod` b)  

-- gcdReverse :: Int -> Int -> Writer [String] Int
-- gcdReverse a b
--  | b == 0    = do
--     tell ["Finished with " ++ show a]
--     return a
--  | otherwise = do
--     result <- gcdReverse b (a `mod` b) 
--     tell  [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
--     return result;

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (f . g)
instance Monoid (DiffList a) where
   mempty = DiffList ([] ++)

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
 | b == 0    = do
    tell $ toDiffList ["Finished with " ++ show a]
    return a
 | otherwise = do
    result <- gcdReverse' b (a `mod` b) 
    tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result;    

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
   tell (toDiffList ["0"])
finalCountDown x = do
   finalCountDown (x - 1)
   tell (toDiffList [show x])

-- finalCountDown :: Int -> Writer [String] ()
-- finalCountDown 0 = do
--    tell ["0"]
-- finalCountDown x = do
--    finalCountDown (x - 1)
--    tell [show x]

addStuff :: Int -> Int
addStuff = do
   a <- (*2)
   b <- (+10)
   return (a + b)

-- addStuff :: Int -> Int
-- addStuff x = let
--    a = (*2) x 
--    b = (+10) x
--    in (a + b)   


type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x, xs)

-- push :: Int -> Stack -> ((), Stack)
-- push a xs  = ((), a:xs)

-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack = let
--    ((), newStack1) = push 3 stack
--    (a, newStack2) = pop newStack1
--    in pop newStack2

-- pop :: State Stack Int
-- pop = state $ \(x:xs) -> (x, xs)

-- push :: Int -> State Stack ()
-- push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
   push 3
   a <- pop
   pop

stackStuff :: State Stack ()
stackStuff = do
   a <- pop
   if a == 5
      then push 5
      else do
         push 3
         push 8

moreStack :: State Stack ()
moreStack = do
   a <- stackManip
   -- if a == 1000
   --    then stackStuff
   --    else return ()
   when (a == 1000) stackStuff

stacklyStack :: State Stack ()   
stacklyStack = do
   stackNow <- get
   if stackNow == [1,2,3]
      then put [8,3,1]
      else put [9,2,1]      

pop :: State Stack Int
pop = do
   xl <- get
   case xl of
      (x:xs) -> do
            put xs
            return x
      _  -> error "Error"

push :: Int -> State Stack ()
push x = do
   xs <- get
   put (x:xs)


keepSmall :: Int -> Writer [String] Bool
keepSmall x
 | x < 4 = do
   tell ["Keeping" ++ show x]
   return True
 | otherwise = do
   tell [show x ++ " is too large, throwing it away"]
   return False

powerset:: [a] -> [[a]]
powerset = filterM (const [True, False])

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
 | x > 9     = Nothing
 | otherwise = Just (acc + x)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing 

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y * x):ys)
foldingFunction (x:y:ys) "+" = return ((y + x):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
   [result] <- foldM foldingFunction [] (words st)
   return result

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving Show

instance Functor Prob where
   fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [
   (Prob [('a', 1%2), ('b', 1%2)], 1 % 4),
   (Prob [('c', 1%2), ('d', 1%2)], 3 % 4)      
  ]   

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x, r)->(x, p*r)) innerxs  

instance Applicative Prob where
   pure x = Prob [(x, 1%1)]  
   (Prob xs) <*> (Prob ys) = Prob [(f v, rx * ry)|(f, rx) <- xs,(v, ry) <-ys]

instance Monad Prob where
   -- return x = Prob [(x, 1%1)]  
   m >>= f = flatten (fmap f m)
   -- fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)
coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
   a <- coin
   b <- coin
   c <- loadedCoin
   return (all (==Tails) [a, b, c])