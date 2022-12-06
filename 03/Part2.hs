module Part2 where

import qualified Data.Set as Set
import Common

getIntersectedElements :: String -> String -> String -> Char
getIntersectedElements s1 s2 s3  = 
    let badge = Set.toList $ (Set.fromList s1) `Set.intersection` (Set.fromList s2) `Set.intersection` (Set.fromList s3) in
    case badge of
        (c:[]) -> c
        _ -> error "bad input"

getBadges :: [String] -> String
getBadges s = getBadges' s "" where
    getBadges' :: [String] -> String -> String 
    getBadges' (s1:s2:s3:xs) acc = getBadges' xs ((getIntersectedElements s1 s2 s3):acc)
    getBadges' [] acc = acc  

main :: IO Int
main = do
    ruckSacks <- readInputFile
    return $ sum $ map getPriority (getBadges ruckSacks)