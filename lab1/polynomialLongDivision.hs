-- Algorithm: A = B*Q+R
-- 1. determines Q: qs2
-- 2. multiplies B with Q
-- 3. substract B*Q from A
-- 4. keeps doing steps 1-3 until you can no longer determine a Q (divide A by B)
-- All the quotients need to be added together; extend the quotientList. Therefore it needs an extra list parameter (qs)
polDivision :: [Double] -> [Double] -> ([Double],[Double])
polDivision ps1 ps2 = doIt ps1 ps2 []
    where doIt :: [Double] -> [Double] -> [Double] -> ([Double],[Double])
          doIt ps1 ps2 qs
            | determinePower ps1 ps2 < 0 = (qs, ps1)
            | otherwise = doIt (subPols ps1 (multPols qs2 ps2)) ps2 (addPols qs qs2)
                where
                    qs2 = quotientList ps1 ps2

-- Determines the power of the dividend of polynomial 1 and polynomial 2
determinePower :: [Double] -> [Double] -> Int
determinePower ps1 ps2 = length ps1 - (length ps2)

-- Initializes the quotientList by filling it with zeros. Then is makes use of the makeList function.
-- After making the list the list is reversed in order to get the right quotient polynomial
quotientList :: [Double] -> [Double] -> [Double]
quotientList ps1 ps2 = reverse (makeList ps1 ps2 (zeroList (determinePower ps1 ps2)))
    where zeroList :: Int -> [Double]
          zeroList len = replicate len 0

--Puts the quotient (dividend of polynomial 1 and polynomial 2) in the reversed correct position of the list
makeList :: [Double] -> [Double] -> [Double] -> [Double]
makeList ps1 ps2 qs = buildList qs ((head ps1) / (head ps2)) (determinePower ps1 ps2)
    where buildList :: [Double] -> Double -> Int -> [Double]
          buildList ys num pos = (take pos ys) ++ [num] ++ drop (pos+1) ys

-- Multiplies two polynomials. If the second polynomial only has 1 value, the map function can be applied to the first polynomial with this value.
-- Otherwise you first shift the polynomials to be in the same order by adding zeroes to the list.
-- Then you multiply an element of the second list with all the elements of first list and add them together untill the second list is empty
multPols :: [Double] -> [Double] -> [Double]
multPols xs [] = xs
multPols xs (y:[]) = map (*y) xs
multPols xs (y:ys) = addPols (map (*y) (shiftPol xs (length ys))) (multPols xs ys)
    where shiftPol :: [Double] -> Int -> [Double]
          shiftPol xs 0 = xs
          shiftPol xs len = shiftPol (xs ++ [0]) (len -1)

-- Adds two polynomials. If the first list is bigger than the other, 0's are added. If the second list is smaller the order of the list is reversed.
-- Eventually the lists have the same length thus the function zipWith (+) can be applied
addPols ::[Double] -> [Double] -> [Double]
addPols xs [] = xs
addPols [] ys = ys
addPols xs ys | length xs > length ys  = addPols xs (0:ys)
              | length xs == length ys = zipWith (+) xs ys
              | otherwise              = addPols ys xs

-- Substracts to polynomials. It uses the addPols function but the second list converted into its negative variant.
-- The 0's are removed from the list so it can decrease
subPols :: [Double] -> [Double] -> [Double]
subPols xs ys = dropWhile (== 0) (addPols xs (map (*(-1)) ys))

wrapper :: String -> ([Double],[Double])
wrapper line = polDivision (makeList num) (makeList denom)
  where
    num = takeWhile (/= '/') line
    denom = tail (dropWhile (/= '/') line)
    makeList str = map (\s -> read s::Double) (words str)

main =  print . wrapper =<< getLine
