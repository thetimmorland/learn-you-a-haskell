removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]

twenty = read "20" :: Int
