module Common where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Char (ord)

getPriority :: Char -> Int
getPriority c = if c < 'a' 
    then ord c - ord 'A' + 27
    else ord c - ord 'a' + 1


readInputFile :: IO [String]
readInputFile = do
    input_lines <- fmap Text.lines (Text.readFile "input.txt")
    return $ map Text.unpack input_lines