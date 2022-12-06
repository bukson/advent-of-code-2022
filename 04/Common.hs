module Common where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

readInputFile :: IO [String]
readInputFile = do
    input_lines <- fmap Text.lines (Text.readFile "input.txt")
    return $ map Text.unpack input_lines

splitStr :: String -> Char -> [String]
splitStr s c =  splitStr' s c "" [] where
    splitStr' :: String -> Char -> String -> [String] -> [String]
    splitStr' [] _ acc result = reverse ((reverse acc):result)
    splitStr' (cs:s) c acc result =  if cs == c 
        then splitStr' s c "" ((reverse acc):result)
        else splitStr' s c (cs:acc) result  

splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s''
    where (w, s'') = break (== c) s'
