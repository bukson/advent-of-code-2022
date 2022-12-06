module Part2 where

import Common

ifOverlaps:: (Int,Int,Int,Int) -> Int
ifOverlaps (l1, r1, l2, r2)  | l1 > l2 = ifOverlaps (l2, r2, l1, r1) 
                            | l1 == l2 = 1
                            | l1 < l2 = if (l2 <= r1) then 1 else 0

main :: IO Int
main = do
    section_pairs <- readInputFile
    return $ sum $ map (ifOverlaps . mapLine) section_pairs