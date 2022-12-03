module Part2 where

import Common
import Data.List(sort)

main :: IO Int
main = do
    strList <- readInputFile
    let sums = map sum (splitStrList strList)
    return $ sum $ drop (length sums - 3) (sort sums) 
