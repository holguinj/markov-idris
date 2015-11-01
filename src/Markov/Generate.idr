module Generate

import Data.SortedMap
import Data.Vect
import Effects
import Effect.Random

import Markov.MarkovMap

||| Given a word and a MarkovMap, selects a word that could follow at random.
nextWord : (word : String) -> MarkovMap -> Eff (Maybe String) [RND]
nextWord word mmap =
  case lookup word mmap of
    Nothing    => pure Nothing -- cool
    Just words => rndSelect words

||| Returns True iff the word starts with an uppercase letter.
isStartWord : String -> Bool
isStartWord word = case unpack word of
                        [] => False
                        (l::_) => isUpper l

||| Returns True iff the word ends in `.`, `?`, or `!`
isEndWord : String -> Bool
isEndWord word = case unpack word of
                      [] => False
                      chars@(_::_) => elem (last chars) endChars
                 where
                   endChars : List Char
                   endChars = ['.', '?', '!']

||| Given a MarkovMap, return a list of words that are valid starters.
markovStartingWords : MarkovMap -> List String
markovStartingWords = filter isStartWord . map fst . toList

||| Return a single random starting word for a given MarkovMap.
rndStart : MarkovMap -> Eff (Maybe String) [RND]
rndStart = rndSelect . markovStartingWords

getWords' : Nat -> (acc : Vect (S n) String) -> MarkovMap -> Eff (List String) [RND]
getWords' Z acc _ = pure $ toList acc
getWords' (S k) acc mmap =
  let word = last acc in
    do case !(nextWord word mmap) of
            Nothing => getWords' Z acc mmap
            Just word => getWords' (if isEndWord word then Z else k) (acc ++ [word]) mmap

getWords : Nat -> MarkovMap -> Eff (List String) [RND]
getWords n mmap = case !(rndStart mmap) of
                       Nothing => pure ["Empty markov map!"]
                       Just start => getWords' n [start] mmap

