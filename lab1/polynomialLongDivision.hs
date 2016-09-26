polDivision :: [Double] -> [Double] -> ([Double],[Double])
polDivision ps1 ps2 = doIt ps1 ps2 []
    where doIt :: [Double] -> [Double] -> [Double] -> ([Double],[Double])
          doIt ps1 ps2 qs
            | determinePower ps1 ps2 < 0 = (qs, ps1)
            | otherwise = doIt (subPols ps1 (multPols qs2 ps2)) ps2 (addPols qs qs2)
                where
                    qs2 = quotientList ps1 ps2

determinePower :: [Double] -> [Double] -> Int
determinePower ps1 ps2 = length ps1 - (length ps2)

quotientList :: [Double] -> [Double] -> [Double]
quotientList ps1 ps2 = reverse (makeList ps1 ps2 (zeroList (determinePower ps1 ps2)))
    where zeroList :: Int -> [Double]
          zeroList len = replicate len 0

makeList :: [Double] -> [Double] -> [Double] -> [Double]
makeList ps1 ps2 qs = buildList qs ((head ps1) / (head ps2)) (determinePower ps1 ps2)
    where buildList :: [Double] -> Double -> Int -> [Double]
          buildList ys num pos = (take pos ys) ++ [num] ++ drop (pos+1) ys

multPols :: [Double] -> [Double] -> [Double]
multPols xs [] = xs
multPols xs (y:[]) = map (*y) xs
multPols xs (y:ys) = addPols (map (*y) (shiftPol xs (length ys))) (multPols xs ys)
    where shiftPol :: [Double] -> Int -> [Double]
          shiftPol xs 0 = xs
          shiftPol xs len = shiftPol (xs ++ [0]) (len -1)

addPols ::[Double] -> [Double] -> [Double]
addPols xs [] = xs
addPols [] ys = ys
addPols xs ys | length xs > length ys  = addPols xs (0:ys)
              | length xs == length ys = zipWith (+) xs ys
              | otherwise              = addPols ys xs

subPols :: [Double] -> [Double] -> [Double]
subPols xs ys = dropWhile (== 0) (addPols xs (map (*(-1)) ys))

wrapper :: String -> ([Double],[Double])
wrapper line = polDivision (makeList num) (makeList denom)
  where
    num = takeWhile (/= '/') line
    denom = tail (dropWhile (/= '/') line)
    makeList str = map (\s -> read s::Double) (words str)

main =  print . wrapper =<< getLine
