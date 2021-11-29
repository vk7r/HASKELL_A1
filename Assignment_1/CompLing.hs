
-- ASSIGNMENT 1 - Oliver Hansson & Viktor Kangasniemi


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

{- mergeDoc doc

VARIANT: 
-}
mergeDoc :: Document -> Sentence
mergeDoc [] = []
mergeDoc (x:xs) =
  x ++ mergeDoc xs

{- strInLst

VARIANT:
-}
strInLst ::  String -> Sentence -> Int -- byt till bättre namn
strInLst str [] = 0
strInLst str (x:xs)
  | str == x = 1 + strInLst str xs
  | otherwise = 0 + strInLst str xs


{- removeStrFromLst
-}
removeStrFromLst :: String -> Sentence -> Sentence
removeStrFromLst str lst = [x | x <- lst, x /= str]

{- wordCount

VARIANT:
-}
wordCountInLst :: Sentence -> WordTally
wordCountInLst [] = []
wordCountInLst (x:xs) = [(x, strInLst x (x:xs) )] ++ wordCountInLst (removeStrFromLst x xs)

{- wordCount
CAPITAL SENSITIVE
-}
wordCount :: Document -> WordTally
wordCount doc = wordCountInLst (mergeDoc doc)


-- 2 ##########################################################################

{- pairsInLst

VARIANT:
-}
pairsInLst :: Sentence -> Pairs
pairsInLst [] = []
pairsInLst [_] = []
pairsInLst (x:y:[]) = [(x, y)]
pairsInLst (x:y:xs) = [(x, y)] ++ pairsInLst (y:xs)

{- adjacentPairs

VARIANT:
-}
adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs [x] = pairsInLst x
adjacentPairs (x:xs) = pairsInLst x ++ adjacentPairs xs


-- 3 ##########################################################################

{- getInitialPairOfLst
-}
getInitialPairOfLst :: Sentence -> Pairs
getInitialPairOfLst [] = []
getInitialPairOfLst [x] = []
getInitialPairOfLst (x:y:xs) = [(x, y)]


{- getFinalPairOfLst
-}
getFinalPairOfLst :: Sentence -> Pairs
getFinalPairOfLst [] = []
getFinalPairOfLst [x] = []
getFinalPairOfLst lst = [( lst !! (length lst - 2) , lst !! (length lst - 1))]


{-initialPairs

VARIANT:
-}
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (x:xs) = getInitialPairOfLst x ++ initialPairs xs

{- finalPairs

VARIANT:
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (x:xs) = getFinalPairOfLst x ++ finalPairs xs


-- 4 ##########################################################################

{- isPair
-}
isPair :: (String, String) -> (String, String) -> Bool
isPair (a,b) (x,y) = if (a == x || a == y) && (b == x || b == y)
  then True
  else False

{-removePairFromPairs
-}
removePairFromPairs :: (String, String) -> Pairs -> Pairs
removePairFromPairs (str1, str2) pairs =
  [x | x <- pairs, not (isPair (str1, str2) x)]

{- pairsCountAUX

VARIANT:
-}
pairsCountAUX :: (String, String) -> Pairs -> Int
pairsCountAUX (_,_) [] = 0
pairsCountAUX (str1, str2) (x:xs)
  | isPair (str1, str2) x = 1 + pairsCountAUX (str1, str2) xs  -- om det är samma par
  | otherwise = 0 + pairsCountAUX (str1, str2) xs

{- pairsCount

VARIANT:
-}
pairsCount :: Pairs -> PairsTally
pairsCount [] = []
pairsCount [x] = [((x), 1)]
pairsCount (x:xs) = [(x, pairsCountAUX x (x:xs) )] ++ pairsCount (removePairFromPairs x xs)



-- 5 ##########################################################################

-- BEÖVER VI CORNER CASES FÖR ALLA FUNKTIONER ELLER KAN VI FÖRKLARA PRECONDITIONS?

{- wordInTally
-}
wordInTally :: (String, String) -> String -> Bool -- KAN SKRIVAS OM INOM ANNAN FUNK
wordInTally (str1, str2) word =
  if word == str1 || word == str2
    then True
    else False

{- getNeighbour
-}
getNeighbour :: (String, String) -> String -> String
getNeighbour (str1, str2) word =
  if word == str1
    then str2
    else str1

{- neighbourAUX
-}
neighbourAUX :: ((String, String), Int) -> String -> WordTally
neighbourAUX ((str1, str2), n) word
  | wordInTally (str1, str2) word = [(getNeighbour (str1, str2) word, n)]
  | otherwise = []

{- neighbours
SKA MAN GÖRA ETT ERROR MEDDELANDE ISTÄLLET FÖR ATT TA EN TOM LISTA?

VARIANT:
-}
neighbours :: PairsTally -> String -> WordTally
neighbours [] word = []
neighbours [x] word = neighbourAUX x word
neighbours (x:xs) word = neighbourAUX x word ++ neighbours xs word


-- 6 ##########################################################################

-- HAR EJ KOLLAT CORNER CASES ALLS PÅ DENNA

{- wordInTally2

VARIANT:
-}
wordInTally2 :: PairsTally -> String -> Bool -- KAN MAN ANVÄNDA DEN FÖRSTA WORD IN TALLY FUNKTIONEN
wordInTally2 [] word = False
wordInTally2 (x:xs) word
  | word == str1 || word == str2 = True
  | otherwise = wordInTally2 xs word
  where
    ((str1, str2), n) = x

{- getPairNum
-}
getPairNum :: (String, Int) -> Int
getPairNum (str, n) = n

{- getPairString
-}
getPairString :: (String, Int) -> String
getPairString (str, n) = str

{- mostCommonNeighbourAUX

VARIANT:
-}
mostCommonNeighbourAUX :: WordTally -> String
mostCommonNeighbourAUX [] = ""
mostCommonNeighbourAUX [x] = getPairString x
mostCommonNeighbourAUX (x:y:xs)
  | getPairNum x >= getPairNum y = mostCommonNeighbourAUX (x:xs)
  | otherwise = mostCommonNeighbourAUX (y:xs)


{- mostCommonNeighbour
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
--mostCommonNeighbour [] word = Nothing
mostCommonNeighbour pTally word
  | wordInTally2 pTally word = Just (mostCommonNeighbourAUX (neighbours pTally word))
  | otherwise = Nothing


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
