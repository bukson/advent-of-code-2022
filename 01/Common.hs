module Common where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

readInputFile :: IO [String]
readInputFile = do
    input_lines <- fmap Text.lines (Text.readFile "input.txt")
    return $ map Text.unpack input_lines

splitStrList :: [String] -> [[Int]]
splitStrList strList = splitStrListAcc strList [] [[]]

splitStrListAcc :: [String] -> [Int] -> [[Int]] -> [[Int]]
splitStrListAcc ("":listTail) acc1 acc2 = splitStrListAcc listTail [] (acc1:acc2)
splitStrListAcc (str:listTail) acc1 acc2 = splitStrListAcc listTail ((read str):acc1) acc2
splitStrListAcc [] acc1 acc2 = (acc1:acc2)