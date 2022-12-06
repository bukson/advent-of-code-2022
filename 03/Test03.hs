module Test03 where

import Test.HUnit
import Common
import Part2

testGetPriorityr = TestCase $ assertEqual "getPriority r = 18" 18 (getPriority 'r')
testGetPriorityZ = TestCase $ assertEqual "getPriority Z = 52" 52 (getPriority 'Z')

testGetIntersectedElements1 = TestCase $ assertEqual 
                                        "getIntersectedElements1" 
                                        'r'
                                        (getIntersectedElements "vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg") 

testGetIntersectedElements2 = TestCase $ assertEqual 
                                        "getIntersectedElements2" 
                                        'Z'
                                        (getIntersectedElements "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" "ttgJtRGJQctTZtZT" "CrZsJsPPZsGzwwsLwLmpwMDw") 

testlist = TestList [
                TestLabel "testGetPriorityr" testGetPriorityr,
                TestLabel "testGetPriorityZ" testGetPriorityZ,
                TestLabel "testGetIntersectedElements1" testGetIntersectedElements1,
                TestLabel "testGetIntersectedElements2" testGetIntersectedElements2]

main :: IO ()
main = do
  runTestTT testlist
  return ()