doubleMe x = x + x
doubleUs x y = doubleMe 7 + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2

list = 1 : 2: 3 : []

extractIdx l i = l !! i

range = [1..100]
odd_ = [1,3..]

congruent3mod7 = [ x | x <- [1..50], x `mod` 7 == 3]

fizzBuzz = [ fb x | x <- [1..100] ]
    where
        fb x
           | congruent 3 = "fizz"
           | congruent 5 = "buzz"
           | congruent 15 = "fizzbuzz"
           | otherwise = show x
           where congruent n = x `mod` n == 0

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
evenXss = [ [ x | x <- xs, even x ] |  xs <- xxs ]