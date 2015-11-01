module Markov.File

import Effects
import Effect.File

readText : (fname : String) -> Eff (Either (IO ()) String) [FILE_IO ()]
readText fname = readFile errFunc fname
                 where
                   errFunc : String -> IO ()
                   errFunc msg = do putStr $ "Unable to read the file '" ++ fname
                                    putStrLn $ "' due to the following error: " ++ msg
