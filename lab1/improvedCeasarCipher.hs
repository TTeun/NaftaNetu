--starts from 'A' == 0 -> 'Z' == 25 for shift == 0
shiftChar :: Int -> Char -> Char
shiftChar sft c = toEnum ((toNum sft c) + 65)::Char
                    where toNum :: Int -> Char -> Int
                          toNum sft c = ((fromEnum c - sft) - 65) `mod` 26

chipherEncode :: Int -> String -> String
chipherEncode sft ss = encodeIt sft ss sft
    where
        encodeIt _ "" _ = ""
        encodeIt sft (' ':ss) orShft = ' ':(encodeIt sft ss orShft)
        encodeIt sft (s:ss) orShft = (sftChar sft s):(encodeIt (sft+orShft) ss orShft)

chipherDecode :: Int -> String -> String
chipherDecode sft ss = chipherEncode (-sft) ss
