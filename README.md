# markov-idris

A simple [Markov generator](https://en.wikipedia.org/wiki/Markov_chain#Markov_text_generators) written in [Idris](http://idris-lang.org).

## building

* Requires [Idris 0.9.19](http://www.idris-lang.org/idris-0-9-19-released/).
* Tested on OS X and Linux, but should run anywhere that Idris does.

Just clone this repository, `cd` into it and run `make`.
This will create an executable called `markov`.

## use

The `markov` binary has a built-in generator for sentences inspired by [The Library of Babel](https://archive.org/stream/TheLibraryOfBabel/babel_djvu.txt), so if you want one of those then you can just run `./markov`.

To generate some text from a file of your choosing, try `./markov load [PATH]`.
Note that this segfaults when reading some larger files (around 4,500+ lines). See [issue #1](https://github.com/holguinj/markov-idris/issues/1).

To see a human-readable version of the default Markov map, try `./markov map`.

For help text, try `./markov help`.

## license

This software is licensed under the GNU General Public License version 3.
See the LICENSE file for more details.
