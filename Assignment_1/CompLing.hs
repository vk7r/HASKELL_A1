
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

-- 1 ########################################################################## JAG

{- mergeDoc doc
   Converts a Document into a single list containing all strings
   RETURNS: A list containing all strings that were in the document
   EXAMPLES: mergeDoc [["a","b","c"], ["d","e"]] == ["a","b","c","d","e",]
             mergeDoc [[a], []] == ["a"]
             mergeDoc [[], []] = []
-}
mergeDoc :: Document -> Sentence
-- VARIANT: length xs
mergeDoc [] = []
mergeDoc (x:xs) =
  x ++ mergeDoc xs


{- strCountInLst str sent
   counts the occurence of a given string in a given list (case sensitive)
   RETURNS: An Int representing the amount of occurences the given word has in the given list
   EXAMPLES: strCountInLst "hello" ["hello","hi", "hello", "HELLO"] == 2
             strCountInLst "bye" ["hello", "hi"] == 0
             strCountInLst "hi" [] == 0
-}
strCountInLst ::  String -> Sentence -> Int -- byt till bättre namn
-- VARIANT: length xs
strCountInLst str [] = 0
strCountInLst str (x:xs)
  | str == x = 1 + strCountInLst str xs
  | otherwise = 0 + strCountInLst str xs


{- removeStrFromLst str lst
   Removes the given string from the given list
   PRE: str must exist within lst
   RETURNS: A list not containing the given string
   EXAMPLES: removeStrFromLst "foo" ["foo", "bar"] == ["bar"]
             removeStrFromLst "foo" [] == []
-}
removeStrFromLst :: String -> Sentence -> Sentence
removeStrFromLst str lst = [x | x <- lst, x /= str]

{- wordCountInLst sent
   Creates a WordTally containing information about the occurence
   of each respective string within a Sentence
   RETURNS: A list of tuples that contain a string alongside an Int
            that represents the number of times the respective string appears
            in the list
   EXAMPLES: wordCountInLst ["foo","bar","baz","foo"] == [("foo", 2), ("bar", 1), ("baz",1 )]
             wordCountInLst ["foo"] == [("foo", 1)]
             wordCountInLst [] == []
-}
wordCountInLst :: Sentence -> WordTally
-- VARIANT: the amount of unique strings within the Sentence (i.e. ["foo", "bar", "foo"] gives the variant 2)
wordCountInLst [] = []
wordCountInLst (x:xs) = [(x, strCountInLst x (x:xs) )] ++ wordCountInLst (removeStrFromLst x xs)


{- wordCount doc
   Creates a WordTally containing information about the occurence
   of each respective string within a Document
   RETURNS: A list of tuples that contain a string alongside an Int
            that represents the number of times the respective string appears
            in the list

   EXAMPLES: wordCount [["foo","foo","bar"], ["bar","baz"]] == [("foo", 2),("bar", 2), ("baz", 1)]
             wordCount [] == []
-}
wordCount :: Document -> WordTally
wordCount doc = wordCountInLst (mergeDoc doc)


-- 2 ########################################################################## OLI

{- pairsInLst

-}
pairsInLst :: Sentence -> Pairs
-- VARIANT:
pairsInLst [] = []
pairsInLst [_] = []
pairsInLst (x:y:[]) = [(x, y)]
pairsInLst (x:y:xs) = [(x, y)] ++ pairsInLst (y:xs)

{- adjacentPairs

-}
adjacentPairs :: Document -> Pairs
-- VARIANT:
adjacentPairs [] = []
adjacentPairs [x] = pairsInLst x
adjacentPairs (x:xs) = pairsInLst x ++ adjacentPairs xs


-- 3 ########################################################################## JAG

{- initialPairsAUX sent
   Takes the first two elements of a given list
   RETURNS: A list containing 1 tuple that contains the first and second element from the given list
   EXAMPLES:initialPairsAUX ["foo", "bar","baz"] == [("foo","bar")]
            initialPairsAUX ["foo","bar"] = [("foo","bar")]
            initialPairsAUX ["foo"] = []
            initialPairsAUX [] == []
-}
initialPairsAUX :: Sentence -> Pairs
initialPairsAUX [] = []
initialPairsAUX [x] = []
initialPairsAUX (x:y:xs) = [(x, y)]


{- finalPairsAUX sent
   Takes the last two elements of a list
   RETURNS: A list containing 1 tuple that  contains the penultimate and last element from the given list
   EXAMPLES: finalPairsAUX ["foo", "bar","baz"] == [("bar","baz")]
             finalPairsAUX ["foo", "bar"] == [("foo","bar")]
             finalPairsAUX ["foo"] = []
             finalPairsAUX [] == []
-}
finalPairsAUX :: Sentence -> Pairs
finalPairsAUX [] = []
finalPairsAUX [x] = []
finalPairsAUX lst = [( lst !! (length lst - 2) , lst !! (length lst - 1))]


{- initialPairs doc
   takes all initial pairs of each list within the Document
   RETURNS: a list containing multiple tuples that consists of every initial pair from the Documents lists
   EXAMPLES: initialPairs [["foo","bar","baz"],["hi","hello"]] == [("foo","bar"), ("hi","hello")]
             initialPairs [[],["foo","bar","baz"]] == [("foo","bar")]
             initialPairs [["foo"],["bar"]] == []
             initialPairs [] == []
-}
initialPairs :: Document -> Pairs
-- VARIANT: length xs
initialPairs [] = []
initialPairs (x:xs) = initialPairsAUX x ++ initialPairs xs

{- finalPairs doc
   takes all final pairs of each list within the Document
   RETURNS: a list containing multiple typles that consists of every final pair from the Documents lists
   EXAMPLES: finalPairs [["foo","bar","baz"],["hi","hello"]] == [("bar","baz"),("hi","hello")]
             finalPairs [[],["foo","bar","baz"]] == [("bar","baz")]
             finalPairs [["foo"],["bar"]] == []
             finalPairs [] == []
-}
finalPairs :: Document -> Pairs
-- VARIANT: length xs
finalPairs [] = []
finalPairs (x:xs) = finalPairsAUX x ++ finalPairs xs


-- 4 ########################################################################## OLI

{- isPair
-}
isPair :: (String, String) -> (String, String) -> Bool
isPair (a,b) (x,y) = if (a == x && b == y) || (a == y && b == x)
  then True
  else False

{-removePairFromPairs
-}
removePairFromPairs :: (String, String) -> Pairs -> Pairs
removePairFromPairs (str1, str2) pairs =
  [x | x <- pairs, not (isPair (str1, str2) x)]

{- pairsCountAUX

-}
pairsCountAUX :: (String, String) -> Pairs -> Int
-- VARIANT:
pairsCountAUX (_,_) [] = 0
pairsCountAUX (str1, str2) (x:xs)
  | isPair (str1, str2) x = 1 + pairsCountAUX (str1, str2) xs  -- om det är samma par
  | otherwise = 0 + pairsCountAUX (str1, str2) xs

{- pairsCount

-}
pairsCount :: Pairs -> PairsTally
-- VARIANT:
pairsCount [] = []
pairsCount [x] = [((x), 1)]
pairsCount (x:xs) = [(x, pairsCountAUX x (x:xs) )] ++ pairsCount (removePairFromPairs x xs)



-- 5 ########################################################################## JAG

-- BEÖVER VI CORNER CASES FÖR ALLA FUNKTIONER ELLER KAN VI FÖRKLARA PRECONDITIONS?

{- wordInTally strPair
   Checks if a given string is in a given tuple of 2 strings
   RETURNS: A bool, returns True if the word is in the tuple otherwise it returns False
   EXAMPLES: wordInTally ("foo","bar") "bar" == true
             wordInTally ("foo","bar") "baz" == False
             wordInTally ("foo","") "" == True
-}
wordInTally :: (String, String) -> String -> Bool
wordInTally (str1, str2) word =
  if word == str1 || word == str2
    then True
    else False

{- getNeighbour strPair word
   If the given string is within the given tuple of 2 strings,
   it takes the words neighbour within the tuple
   PRE: The given word must be within the given tuple of 2 strings
   RETURNS: a string, that is the neighbour of the given string within the tuple
   EXAMPLES: getNeighbour ("foo","bar") "foo" == "bar"
             getNeighbour ("foo","foo") "foo" == "foo"
-}
getNeighbour :: (String, String) -> String -> String
getNeighbour (str1, str2) word =
  if word == str1
    then str2
    else str1

{- neighbourAUX PTallyEl word
  Takes in an element from a PairsTally and gives information about the
  neighbour-string if the given word is in the tuple of 2 strings.
  RETURNS: If the word is in the tuple it returns a WordTally containing
           the neighbour-string and an Int representing the occurence of
           the neighbour-string
  EXAMPLES: neighbourAUX (("foo","bar"), 2) "foo" == [("bar", 2)]
            neighbourAUX (("foo","bar"), 2) "baz" == []
-}
neighbourAUX :: ((String, String), Int) -> String -> WordTally
neighbourAUX ((str1, str2), n) word
  | wordInTally (str1, str2) word = [(getNeighbour (str1, str2) word, n)]
  | otherwise = []

{- neighbours PTally word
   takes a PairsTally and a word and gives information about all neighbours
   of the given word, showcasing the occurance of each neighbour-string.
   RETURNS: A list of tuples that contain each neighbour-string of the given words
            along side an Int that represents the occurence of the neighbour-string
            in the given PairsTally.
   EXAMPLES: neighbours [(("bear","big"),2),(("big","dog"),1)] "big" == [("bear",2),("dog",1)]
             neighbours [(("bear","big"),2),(("big","dog"),1),(("hello","hi"), 1)] "big" == [("bear",2),("dog",1)]
             neighbours [(("bear","big"),2),(("big","dog"),1)] "cow" == []
             neighbours [] "big" == []
-}
neighbours :: PairsTally -> String -> WordTally
-- VARIANT: length xs
neighbours [] word = []
neighbours (x:xs) word = neighbourAUX x word ++ neighbours xs word


-- 6 ########################################################################## OLI

-- HAR EJ KOLLAT CORNER CASES ALLS PÅ DENNA

{- wordInTally2

-}
wordInTally2 :: PairsTally -> String -> Bool -- KAN MAN ANVÄNDA DEN FÖRSTA WORD IN TALLY FUNKTIONEN
-- VARIANT:
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

-}
mostCommonNeighbourAUX :: WordTally -> String
-- VARIANT:
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
