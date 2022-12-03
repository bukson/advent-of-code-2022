module Part1 where

import Common

main :: IO Int
main = do
    strList <- readInputFile
    let sums = map sum (splitStrList strList)
    return $ foldl (\acc i -> max acc i) 0 sums