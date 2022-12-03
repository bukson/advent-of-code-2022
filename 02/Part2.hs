module Part2 where

import Common


getMyScore:: String -> Ordering
getMyScore "X" = GT
getMyScore "Y" = EQ
getMyScore "Z" = LT

getMyPlay:: Play -> Ordering -> Play
getMyPlay oponentPlay myResult = getMyPlay' oponentPlay myResult [Rock, Paper, Scissors] where
    getMyPlay' oponentPlay myResult (p':ps) = 
        if compare oponentPlay p' == myResult 
            then p' 
            else getMyPlay' oponentPlay myResult ps 

getFullScore :: [String] -> Int
getFullScore (opponentPlayString:myScoreString:[]) = 
    let opponentPlay = getOponnetPlay opponentPlayString in 
    let myResult = getMyScore myScoreString in
    let myPlay = getMyPlay opponentPlay myResult in
    mapResult myResult + (getPlayScore myPlay)


main :: IO Int
main = do
    strPairList <- readInputFile
    let totalScore = sum $ map getFullScore strPairList
    return totalScore 
