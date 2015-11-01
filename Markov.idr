module Main

import Data.SortedMap
import Data.Vect
import Effects
import Effect.Random
import Effect.StdIO
import Effect.System
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
buildMarkovMap (base :: rest@(next :: _)) mmap =
  buildMarkovMap rest $ conjWord base next mmap -- not super happy with this

||| Given a word and a MarkovMap, selects a word that could follow at random.
nextWord : (word : String) -> MarkovMap -> { [RND] } Eff (Maybe String)
nextWord word mmap =
  case lookup word mmap of
    Nothing    => pure Nothing -- cool
    Just words => rndSelect words

||| Returns True iff the word starts with an uppercase letter.
isStartWord : String -> Bool
isStartWord word = case unpack word of
                        [] => False
                        (l::_) => isUpper l

||| Returns True iff the word does not end in `.`, `?`, or `!`
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

||| The RND effect doesn't do this for us, so pick something based on the time.
getTimeSeed : Eff Integer [SYSTEM]
getTimeSeed = time

main' : { [STDIO, RND, SYSTEM] } Eff ()
main' = do srand !getTimeSeed
           putStr "Using random seed: "
           putStrLn (show !getTimeSeed)
           case !(rndStart babelMap) of
             Nothing => printLn "Oh shit, nothing to print!"
             Just word => printLn $ unwords $ !(getWords' 10 [word] babelMap)

str : List String -> String
str [] = ""
str strs = foldr Prelude.Strings.(++) "" strs

join : List String -> String -> String
join strs delimeter = str $ intersperse delimeter strs

prettyShow : MarkovMap -> String
prettyShow mmap = let listified = Data.SortedMap.toList mmap in
                      join (map prettyShow' listified) "\n"
                  where
                    prettyShow' : (String, List String) -> String
                    prettyShow' (word, followers) = str $ [word, ": "] ++ [(join followers ", ")]

helpText : String
helpText = "The supported commands are:\n  map -> print the Markov map.\n  help -> print this help."

runCommand : String -> IO ()
runCommand command = case command of
                          "help" => putStrLn helpText
                          "map" => putStrLn $ prettyShow babelMap
                          _ => do putStrLn "That command is not supported."
                                  putStrLn helpText

main : IO ()
main = case !getArgs of
            (_::command::_) => runCommand command
            _ => run main'
