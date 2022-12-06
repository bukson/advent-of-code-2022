module Part1 where

import qualified Data.Set as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Char (ord)

getIntersectedElements :: String -> String
getIntersectedElements s = 
    let (s1,s2) = splitAt (length s `div` 2) s in
    Set.toList $ Set.intersection (Set.fromList s1) (Set.fromList s2)

getPriority :: Char -> Int
getPriority c = if c < 'a' 
    then ord c - ord 'A' + 27
    else ord c - ord 'a' + 1


readInputFile :: IO [String]
readInputFile = do
    input_lines <- fmap Text.lines (Text.readFile "input.txt")
    return $ map Text.unpack input_lines

-- main :: IO Int
main = do
    ruckSacks <- readInputFile
    let elements = map getIntersectedElements ruckSacks
    -- return 0
    return $ sum $ map (sum . map getPriority ) elements