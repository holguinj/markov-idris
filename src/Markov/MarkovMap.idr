module Markov.MarkovMap

import Data.SortedMap

||| A type representing a sorted map of String -> List String
MarkovMap : Type
MarkovMap = SortedMap String (List String)
%name MarkovMap mmap

emptyMarkovMap : MarkovMap
emptyMarkovMap = empty

||| Join a list of strings together using the given delimeter.
join : List String -> String -> String
join strs delimeter = concat $ intersperse delimeter strs

||| Convert a MarkovMap into a human-readable string.
prettyShow : MarkovMap -> String
prettyShow mmap = let listified = Data.SortedMap.toList mmap in
                      unlines (map prettyShow' listified)
                  where
                    prettyShow' : (String, List String) -> String
                    prettyShow' (word, followers) = concat $ [word, ": "] ++ [(join followers ", ")]

instance Show MarkovMap where show = prettyShow
