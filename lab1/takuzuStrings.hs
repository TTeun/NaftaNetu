add0 :: [String] -> [String]
add0 xs = ['0':ss | ss <- xs, head ss == '1' || take 2 ss == "01" ]

add1 :: [String] -> [String]
add1 xs = ['1':ss | ss <- xs, head ss == '0' || take 2 ss == "10" ]

addAll :: [String] -> [String]
addAll xs = add0 xs ++ add1 xs

makeTakuzu :: Int -> [String]
makeTakuzu 0 = []
makeTakuzu 1 = ["0", "1"]
makeTakuzu 2 = ["00", "01", "10", "11"]
makeTakuzu n = addAll (makeTakuzu (n-1))
