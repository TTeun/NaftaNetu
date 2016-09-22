go00, go01, go10, go11 :: [String] -> [String]
go00 bs = ["00" ++ xs | xs <- bs, head xs /= '0'  ]
go01 bs = ["01" ++ xs | xs <- bs, take 2 xs /= "11" ]
go10 bs = ["10" ++ xs | xs <- bs, take 2 xs /= "00" ]
go11 bs = ["11" ++ xs | xs <- bs, head xs /= '1'  ]

takuzuStrings :: Integer -> [String]
takuzuStrings n = makeEm (n `div` 2 - 1) ["00","01","10","11"]
    where
        makeEm 0 bs = bs
        makeEm n bs = makeEm (n - 1) $ go00 bs ++ go01 bs ++ go10 bs ++ go11 bs
