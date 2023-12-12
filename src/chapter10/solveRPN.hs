solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (y * x):ys
        foldingFunction (x:y:ys) "+" = (y + x):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys        
        foldingFunction (x:y:ys) "/" = (y / x):ys        
        foldingFunction (x:y:ys) "^" = (y ** x):ys        
        foldingFunction (x:ys) "ln" = log x:ys        
        foldingFunction xs "sum" = [sum xs]      
        foldingFunction xs numberString = read numberString:xs