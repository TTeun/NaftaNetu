--starts from 'A' == 0 -> 'Z' == 25 for shift == 0
sftChar :: Int -> Char -> Char
sftChar sft c = toEnum ((toNum sft c) + 65)::Char
                    where toNum :: Int -> Char -> Int
                          toNum sft c = ((fromEnum c - sft) - 65) `mod` 26

cipherEncode :: Int -> String -> String
cipherEncode sft ss = encodeIt sft ss sft
    where
        encodeIt _ "" _ = ""
        encodeIt sft (' ':ss) orShft = ' ':(encodeIt sft ss orShft)
        encodeIt sft (s:ss) orShft = (sftChar sft s):(encodeIt (sft+orShft) ss orShft)

cipherDecode :: Int -> String -> String
cipherDecode sft ss = cipherEncode (-sft) ss
