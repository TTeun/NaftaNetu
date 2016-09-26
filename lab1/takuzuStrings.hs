-- A 'zero' can only be added before a '1' or before a '01'
add0 :: [String] -> [String]
add0 xs = ['0':ss | ss <- xs, head ss == '1' || take 2 ss == "01" ]

--- A '1' can only be added before a '0' or before a '10'
add1 :: [String] -> [String]
add1 xs = ['1':ss | ss <- xs, head ss == '0' || take 2 ss == "10" ]

-- This combines the above two
addAll :: [String] -> [String]
addAll xs = add0 xs ++ add1 xs

-- This makes for a certain length all the possible takuzu strings. The first three cases are needed in order for the 'addAll' to work.
takuzuStrings :: Integer -> [String]
takuzuStrings 0 = []
takuzuStrings 1 = ["0", "1"]
takuzuStrings 2 = ["00", "01", "10", "11"]
takuzuStrings n = addAll (takuzuStrings (n-1))

wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
