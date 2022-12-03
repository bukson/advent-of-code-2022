module Part1 where

import Common
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

getMyPlay :: String -> Play
getMyPlay "X" = Rock
getMyPlay "Y" = Paper
getMyPlay "Z" = Scissors

getFullScore :: [String] -> Int
getFullScore (opponentPlayString:myPlayString:[]) = 
    let opponentPlay = getOponnetPlay opponentPlayString in 
    let myPlay = getMyPlay myPlayString in
    let result = compare opponentPlay myPlay in
    mapScore result + (mapPlay myPlay)


main :: IO Int
main = do
    strPairList <- readInputFile
    let totalScore = sum $ map getFullScore strPairList
    return totalScore 
