module Common where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text


data Play = Rock | Paper | Scissors deriving Eq
instance Ord Play where
    compare Rock Paper = LT
    compare Paper Scissors = LT
    compare Scissors Rock = LT
    compare p1 p2 = if p1 == p2 then EQ else reverseOrdering $ compare p2 p1

reverseOrdering :: Ordering -> Ordering
reverseOrdering LT = GT
reverseOrdering GT = LT
reverseOrdering EQ = EQ

getOponnetPlay :: String -> Play
getOponnetPlay "A" = Rock
getOponnetPlay "B" = Paper
getOponnetPlay "C" = Scissors

mapPlay :: Play -> Int
mapPlay Rock = 1
mapPlay Paper = 2
mapPlay Scissors = 3

mapScore :: Ordering -> Int
mapScore LT = 6
mapScore EQ = 3
mapScore GT = 0

readInputFile :: IO [[String]]
readInputFile = do
    input_lines <- fmap Text.lines (Text.readFile "input.txt")
    return $ map (\l -> words (Text.unpack l)) input_lines
