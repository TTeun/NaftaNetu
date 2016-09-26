import Data.Char
-- Transforms a character to a number, shifts it for a given key and transform that number back to the character.
-- Starts from 'A' == 0 -> 'Z' == 25 for shift == 0.
sftChar :: Int -> Char -> Char
sftChar sft c = toEnum ((toNum sft c) + 65)::Char
                    where toNum :: Int -> Char -> Int
                          toNum sft c = ((fromEnum c - sft) - 65) `mod` 26

-- Recursively implements the improved ceaser cipher. The original shift needs to be remember so therefore the actual function has 3 parameters.
cipherEncode :: Int -> String -> String
cipherEncode sft ss = encodeIt sft ss sft
    where
        encodeIt _ "" _ = ""
        encodeIt sft (' ':ss) orShft = ' ':(encodeIt sft ss orShft)
        encodeIt sft (s:ss) orShft = (sftChar sft s):(encodeIt (sft+orShft) ss orShft)

-- The decoder is the same as the encoder but with the negative key.
cipherDecode :: Int -> String -> String
cipherDecode sft ss = cipherEncode (-sft) ss

wrapper :: String -> String
wrapper line
  | cmd == "ENCODE"  = cipherEncode key txt
  | cmd == "DECODE"  = cipherDecode key txt
  where
    str  = dropWhile (not.isAlpha) line
    cmd  = takeWhile isAlpha str
    tail = dropWhile (not.isDigit) str
    key = read (takeWhile isDigit tail)::Int
    txt = dropWhile (not.isAlpha) (dropWhile isDigit tail)

main =  print . wrapper =<< getLine
