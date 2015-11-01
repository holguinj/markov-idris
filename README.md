# Markov-idris

Oh hey, this is a work in progress and it isn't ready for general use.

You can build it, if you like, by running `make` in this directory.
You can then run the resulting `markov` executable.
It should print out a random sentence inspired by [The Library of Babel](https://archive.org/stream/TheLibraryOfBabel/babel_djvu.txt).

To generate some text from a file of your choosing, try `./markov load [PATH]`.
Note that this appears to segfault when reading larger files.

To see a human-readable version of the default Markov map, try `./markov map`.

For help text, try `./markov help`.
