
-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- 1 ##########################################################################

{-
-}
mergeDoc :: Document -> Sentence
mergeDoc [] = []
mergeDoc (x:xs) =
  x ++ mergeDoc xs

{-
-}
strInLst ::  String -> Sentence -> Int -- byt till bättre namn
strInLst str [] = 0
strInLst str (x:xs)
  | str == x = 1 + strInLst str xs
  | otherwise = 0 + strInLst str xs


{-
-}
removeStrFromLst :: String -> Sentence -> Sentence
removeStrFromLst str lst = [x | x <- lst, x /= str]

{-
-}
wordCountInLst :: Sentence -> WordTally
wordCountInLst [] = []
wordCountInLst (x:xs) = [(x, strInLst x (x:xs) )] ++ wordCountInLst (removeStrFromLst x xs)

{-
CAPITAL SENSITIVE
-}
wordCount :: Document -> WordTally
wordCount doc = wordCountInLst (mergeDoc doc)


-- 2 ##########################################################################

{-
-}
pairsInLst :: Sentence -> Pairs
pairsInLst [] = []
pairsInLst [_] = []
pairsInLst (x:y:[]) = [(x, y)]
pairsInLst (x:y:xs) = [(x, y)] ++ pairsInLst (y:xs)

{-
-}
adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs [x] = pairsInLst x
adjacentPairs (x:xs) = pairsInLst x ++ adjacentPairs xs


-- 3 ##########################################################################

{-
-}
getInitialPairOfLst :: Sentence -> Pairs
getInitialPairOfLst [] = []
getInitialPairOfLst [x] = []
getInitialPairOfLst (x:y:xs) = [(x, y)]


{-
-}
getFinalPairOfLst :: Sentence -> Pairs
getFinalPairOfLst [] = []
getFinalPairOfLst [x] = []
getFinalPairOfLst lst = [( lst !! (length lst - 2) , lst !! (length lst - 1))]


{-
-}
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (x:xs) = getInitialPairOfLst x ++ initialPairs xs

{-
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (x:xs) = getFinalPairOfLst x ++ finalPairs xs


-- 4 ##########################################################################

--PRE ATT DET SKA VARA ETT PAR KANSKE ÄNDÅ BLIR ERROR DÅ TYPEN BEHÖVER EXAKT 2 ST
-- isPair :: (String, String) -> (String, String) -> Bool
-- isPair (a:b) (x:y) = if (a == x || a == y) && (b == x || b == y)
--   then
--     True
--   else
--     False
-- {-
-- -}
pairsCount :: Pairs -> PairsTally
pairsCount = undefined

-- pairsCount [] = []
-- pairsCount [x] = [x]
-- pairsCount (x:y:xs)
--   | isPair x y = [((x, y), 23)] ++ pairsCount xs
--   | otherwise = pairsCount xs


-- 5 ##########################################################################
{-
-}
neighbours :: PairsTally -> String -> WordTally
neighbours = undefined  -- remove "undefined" and write your function here


-- 6 ##########################################################################
{-
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here



-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]])

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])

test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])


-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple"
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky"
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a")

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b")

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun")
                                                                  (mostCommonNeighbour input "the")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\""
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\""
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet")

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]
