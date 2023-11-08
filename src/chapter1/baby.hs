doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =  if x > 100
                        then x
                        else x * 2
doubleSmallNumber' x = doubleSmallNumber x + 1

boomBangs :: Integral a => [a] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!"|x<-xs, odd x]

fizzbuzz xs = [
    if (x `mod` 3) == 0 && (x `mod` 5) == 0 then "FizzBuzz" 
    else if x `mod` 3 == 0 then "Fizz"
    else if x `mod` 5 == 0 then "Buzz"
    else show x | x <-xs
    ]

length' xs = sum [1 | _ <-xs]    

removeNonUpperCase st = [c | c<-st, c `elem` ['A'..'Z']]