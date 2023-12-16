import Control.Applicative (Applicative(liftA2))
myAction :: IO String
myAction = (++) <$> getLine <*> getLine

sequenceA' :: (Applicative f) => [f a] -> f [a]
-- sequenceA' [] = pure []
-- sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
sequenceA' = foldr (liftA2 (:)) (pure [])