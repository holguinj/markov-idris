module Markov.Analyze

import Data.SortedMap

import Markov.MarkovMap

||| Adds an entry to a MarkovMap unless that word is already there.
addWord : (baseWord : String) -> (newWord : String) -> MarkovMap -> MarkovMap
addWord baseWord newWord mmap =
  case lookup baseWord mmap of
    Nothing       => insert baseWord [newWord] mmap
    Just oldWords => insert baseWord (ensure newWord oldWords) mmap
  where
    ensure : String -> List String -> List String
    ensure word words = if word `elem` words then words else word :: words

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

||| Progressively populates a (possibly empty) MarkovMap using the given list of strings.
buildMarkovMap' : List String -> MarkovMap -> MarkovMap
buildMarkovMap' [] mmap = mmap -- done
buildMarkovMap' (x :: []) mmap = mmap -- also done
buildMarkovMap' (base :: rest@(next :: _)) mmap =
  buildMarkovMap' rest $ addWord base next mmap -- not super happy with this

||| Builds a brand-new MarkovMap out of a nice, pleasant text.
buildMarkovMap : String -> MarkovMap
buildMarkovMap text = let prepared = map cleanup $ words text in
                          buildMarkovMap' prepared empty
