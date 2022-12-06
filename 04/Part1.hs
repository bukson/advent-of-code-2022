module Part1 where

import Common

mapLine :: String -> (Int,Int,Int,Int)
mapLine s = let (s1:s2:[]) = splitBy ',' s in
    let (l1:r1:[]) = splitBy '-' s1 in
    let (l2:r2:[]) = splitBy '-' s2 in
    (read l1, read r1, read l2, read r2)

ifInRange :: (Int,Int,Int,Int) -> Int
ifInRange (l1, r1, l2, r2)  | l1 > l2 = ifInRange (l2, r2, l1, r1) 
                            | l1 == l2 = 1
                            | l1 < l2 = if (l2 <= r1) && (r2 <= r1) then 1 else 0

main :: IO Int
main = do
    section_pairs <- readInputFile
    return $ sum $ map (ifInRange . mapLine) section_pairs