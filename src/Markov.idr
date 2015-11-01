module Main

import Data.SortedMap
import Data.Vect
import Effects
import Effect.Random
import Effect.StdIO
import Effect.System

import Markov.Example
import Markov.Analyze

-- %default total

||| Concatenate the given list of strings into a single string.
str : List String -> String
str [] = ""
str strs = foldr Prelude.Strings.(++) "" strs

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
rndStart : MarkovMap -> { [RND] } Eff (Maybe String)
rndStart = rndSelect . markovStartingWords

getWords' : Nat -> (acc : Vect (S n) String) -> MarkovMap -> { [RND] } Eff (List String)
getWords' Z acc _ = pure $ toList acc
getWords' (S k) acc mmap =
  let word = last acc in
    do case !(nextWord word mmap) of
            Nothing => getWords' Z acc mmap
            Just word => getWords' (if isEndWord word then Z else k) (acc ++ [word]) mmap

getWords : Nat -> MarkovMap -> { [RND] } Eff (List String)
getWords n mmap = case !(rndStart mmap) of
                       Nothing => pure ["Empty markov map!"]
                       Just start => getWords' n [start] mmap

||| The RND effect doesn't do this for us, so pick something based on the time.
getTimeSeed : Eff Integer [SYSTEM]
getTimeSeed = time

main' : { [STDIO, RND, SYSTEM] } Eff ()
main' = do srand !getTimeSeed
           putStr "Using random seed: "
           putStrLn (show !getTimeSeed)
           printLn $ unwords $ !(getWords 50 babelMap)

||| Join a list of strings together using the given delimeter.
join : List String -> String -> String
join strs delimeter = str $ intersperse delimeter strs

||| Convert a MarkovMap into a human-readable string.
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
