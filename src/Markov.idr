module Main

import Data.SortedMap
import Data.Vect
import Effects
import Effect.Random
import Effect.StdIO
import Effect.System
import Effect.File

import Markov.Analyze
import Markov.Example
import Markov.File
import Markov.Generate
import Markov.MarkovMap

-- %default total

||| The RND effect doesn't do this for us, so pick something based on the time.
getTimeSeed : Eff Integer [SYSTEM]
getTimeSeed = time

main' : MarkovMap -> Eff () [STDIO, RND, SYSTEM]
main' mmap = do srand !getTimeSeed
                putStr "Using random seed: "
                putStrLn (show !getTimeSeed)
                printLn $ unwords $ !(getWords 50 mmap)

markovFromFile' : String -> IO ()
markovFromFile' fname = do fcontents' <- run $ readText fname
                           case fcontents' of
                                Left errMsg => putStrLn errMsg
                                Right fcontents => run $ main' $ buildMarkovMap fcontents

markovFromFile : (args: List String) -> IO ()
markovFromFile [] = putStrLn "Please supply the path to a file to load."
markovFromFile (fname::_) = markovFromFile' fname

helpText : String
helpText = "The supported commands are:\n  " ++ join commands "\n  "
            where
              commands : List String
              commands = [ "load [path] -> generate a sentence from a given file."
                         , "map -> print the default Markov map."
                         , "help -> print this help." ]

runCommand : String -> List String -> IO ()
runCommand command args = case command of
                            "help" => putStrLn helpText
                            "map" => putStrLn $ prettyShow babelMap
                            "load" => markovFromFile args
                            _ => do putStrLn "That command is not supported."
                                    putStrLn helpText

main : IO ()
main = case !getArgs of
            (_::command::args) => runCommand command args
            _ => run $ main' babelMap
