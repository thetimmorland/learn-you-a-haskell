-- main = interact respondPalindrome

-- respondPalindrome :: String -> String
--  respondPalindrome = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines

-- isPalindrome :: (Eq a) => [a] -> Bool
-- isPalindrome xs = xs == reverse xs


-- import System.IO

-- main = do
--     handle <- openFile "girlfriend.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle


import System.IO

main = withFile "girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)