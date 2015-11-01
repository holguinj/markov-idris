module Markov.File

import Effects
import Effect.File

readText : (fname : String) -> Eff (Either String String) [FILE_IO ()]
readText fname = readFile errFunc fname
                 where
                   errFunc : String -> String
                   errFunc fname = "Unable to read the file '" ++ fname ++ "'."
