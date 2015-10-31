module Main

import Data.SortedMap
import Data.Vect
import Effects
import Effect.Random
import Effect.StdIO
import Example

-- %default total

||| A type representing a sorted map of String -> List String
MarkovMap : Type
MarkovMap = SortedMap String (List String)
%name MarkovMap mmap

emptyMarkovMap : MarkovMap
emptyMarkovMap = empty

||| Adds an entry to a markovMap, even if that word is already there.
conjWord : (baseWord : String) -> (newWord : String) -> MarkovMap -> MarkovMap
conjWord baseWord newWord mmap =
    case lookup baseWord mmap of
      Nothing       => insert baseWord [newWord] mmap
      Just oldWords => insert baseWord (newWord :: oldWords) mmap

||| Progressively populates a (possibly empty) MarkovMap using the given list of strings.
buildMarkovMap : List String -> MarkovMap -> MarkovMap
buildMarkovMap [] mmap = mmap -- done
buildMarkovMap (x :: []) mmap = mmap -- also done
buildMarkovMap (base :: (next :: words)) mmap =
  buildMarkovMap words $ conjWord base next mmap -- not super happy with this

||| Given a word and a MarkovMap, selects a word that could follow at random.
nextWord : (word : String) -> MarkovMap -> { [RND] } Eff (Maybe String)
nextWord word mmap =
  case lookup word mmap of
    Nothing    => pure Nothing -- cool
    Just words => rndSelect words

||| Returns True iff the word does not end in `.`
isEndWord : String -> Bool
isEndWord = isSuffixOf "."

||| Given a MarkovMap, return a list of words that are valid starters.
markovStartingWords : MarkovMap -> List String
markovStartingWords = filter (not . isEndWord) . map fst . toList

||| Return a single random starting word for a given MarkovMap.
rndStart : MarkovMap -> { [RND] } Eff (Maybe String)
rndStart = rndSelect . markovStartingWords

babelMap : MarkovMap
babelMap = buildMarkovMap babelWords empty

getWords' : Nat -> (acc : Vect (S n) String) -> MarkovMap -> { [RND] } Eff (List String)
getWords' Z acc _ = pure $ toList acc
getWords' (S k) acc mmap =
  let word = last acc in
    do case !(nextWord word mmap) of
            Nothing => getWords' Z acc mmap
            Just word => getWords' k (acc ++ [word]) mmap

main' : { [STDIO, RND] } Eff ()
main' = do srand 12345888
           case !(rndStart babelMap) of
             Nothing => printLn "Oh shit, nothing to print!"
             Just word => printLn $ unwords $ !(getWords' 7 [word] babelMap)

main : IO ()
main = run main'
