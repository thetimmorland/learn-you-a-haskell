zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where p n = n `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n = n : chain (n `div` 2)
        | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where isLong xs = length xs >= 15

elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: (Ord a) => [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
