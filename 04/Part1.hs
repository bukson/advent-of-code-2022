module Part1 where

import Common

ifContains:: (Int,Int,Int,Int) -> Int
ifContains (l1, r1, l2, r2)  | l1 > l2 = ifContains (l2, r2, l1, r1) 
                            | l1 == l2 = 1
                            | l1 < l2 = if (l2 <= r1) && (r2 <= r1) then 1 else 0

main :: IO Int
main = do
    section_pairs <- readInputFile
    return $ sum $ map (ifContains . mapLine) section_pairs