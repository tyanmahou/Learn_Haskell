import Control.Arrow (ArrowChoice(right), Arrow (first))
import Text.Read (Lexeme(Char))
type Birds = Int
type Pole = (Birds, Birds)

checkPole :: Pole -> Maybe Pole
checkPole (left, right)
  | abs(left - right) < 4 = Just (left, right)
  | otherwise             = Nothing

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) = checkPole (left + n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) = checkPole (left, right + n)

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  a <- banana first
  second <- landRight 2 a
  landLeft 1 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x
wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x  