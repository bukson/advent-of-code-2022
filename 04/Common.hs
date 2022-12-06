module Common where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

readInputFile :: IO [String]
readInputFile = do
    input_lines <- fmap Text.lines (Text.readFile "input.txt")
    return $ map Text.unpack input_lines

splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s''
    where (w, s'') = break (== c) s'

mapLine :: String -> (Int,Int,Int,Int)
mapLine s = let (s1:s2:[]) = splitBy ',' s in
    let (l1:r1:[]) = splitBy '-' s1 in
    let (l2:r2:[]) = splitBy '-' s2 in
    (read l1, read r1, read l2, read r2)
