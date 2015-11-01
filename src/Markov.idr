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

showFile : List String -> IO ()
showFile [] = putStrLn "The 'show' command requires a file path."
showFile (fname::_) = do fcontents' <- run $ readText fname
                         case fcontents' of
                           Left errMsg => putStrLn errMsg
                           Right fcontents => putStrLn fcontents

markovFromFile : List String -> IO ()
markovFromFile [] = putStrLn "The 'load' command requires a file path."
markovFromFile (fname::_) = do fcontents' <- run $ readText fname
                               case fcontents' of
                                 Left errMsg => putStrLn errMsg
                                 Right fcontents => run $ main' $ buildMarkovMap fcontents

helpText : String
helpText = "Call with no arguments to generate a sentence from the default map,"
           ++ " or try one of the following commands:\n  " ++ join commands "\n  "
            where
              commands : List String
              commands = [ "load [path] -> generate a sentence from a given file."
                         , "show [path] -> read the entire file at [path] to stdout. For debugging file IO only."
                         , "map -> print the default Markov map."
                         , "help -> print this help." ]

runCommand : String -> List String -> IO ()
runCommand command args = case command of
                            "load" => markovFromFile args
                            "show" => showFile args
                            "map" => putStrLn $ prettyShow babelMap
                            "help" => putStrLn helpText
                            _ => do putStrLn "That command is not supported."
                                    putStrLn helpText

main : IO ()
main = case !getArgs of
            (_::command::args) => runCommand command args
            _ => run $ main' babelMap
