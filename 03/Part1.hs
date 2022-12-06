module Part1 where

import qualified Data.Set as Set
import Common

getIntersectedElements :: String -> String
getIntersectedElements s = 
    let (s1,s2) = splitAt (length s `div` 2) s in
    Set.toList $ Set.intersection (Set.fromList s1) (Set.fromList s2)

main :: IO Int
main = do
    ruckSacks <- readInputFile
    let elements = map getIntersectedElements ruckSacks
    return $ sum $ map (sum . map getPriority ) elements