maximum' :: (Ord a) => [a] -> a
maximum' []       = error "maximum of empty list"
maximum' [x     ] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Num n, Ord n) => n -> x -> [x]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

take' :: (Num n, Ord n) => n -> [x] -> [x]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x : xs)   = x : take' (n - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' []       _        = []
zip' _        []       = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs) | x == a    = True
                 | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [ a | a <- xs, a <= x ]
      biggerSorted  = quicksort [ a | a <- xs, a > x ]
  in  smallerSorted ++ [x] ++ biggerSorted
