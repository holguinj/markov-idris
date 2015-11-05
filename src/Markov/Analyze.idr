module Markov.Analyze

import Data.SortedMap

import Markov.MarkovMap

||| Remove any/all of the given characters from a string.
remove : List Char -> String -> String
remove chars string = let wordChars = unpack string in
                          pack $ filter (not . (flip elem) chars) wordChars

||| Remove unhelpful characters from a string.
cleanup : String -> String
cleanup = remove badPunctuation
            where
              badPunctuation : List Char
              badPunctuation = [',', ':', ';', '(', ')', '/', '[', ']', '"', '*', '<', '>', '-', '_']

||| A tuple of the form (word, nextWord)
WordPair : Type
WordPair = (String, String)

||| Given a list of words (a text), return a list of WordPairs
wordPairs : List String -> List WordPair
wordPairs [] = []
wordPairs words@(_::tailWords) = zipWith MkPair words tailWords

||| Adds an entry to a MarkovMap, unless that word is already there.
addWord : WordPair -> MarkovMap -> MarkovMap
addWord (baseWord, nextWord) mmap =
  case lookup baseWord mmap of
       Nothing       => insert baseWord [nextWord] mmap
       Just oldWords => insert baseWord (ensure nextWord oldWords) mmap
  where
    ensure : String -> List String -> List String
    ensure word words = if word `elem` words then words else word :: words

||| Progressively populates a (possibly empty) MarkovMap using the given list of strings.
buildMarkovMap' : List String -> MarkovMap -> MarkovMap
buildMarkovMap' [] mmap = mmap
buildMarkovMap' words mmap =
  let pairs = wordPairs words in
    foldr addWord mmap pairs

||| Builds a brand-new MarkovMap out of a nice, pleasant text.
buildMarkovMap : String -> MarkovMap
buildMarkovMap text = let prepared = map cleanup $ words text in
                        buildMarkovMap' prepared empty
