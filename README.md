#hs-adventure-txt
**Simple text adventure engine in Haskell**

This is a simple implementation of the Cadventure-txt format:
http://github.com/tech-no-crat/Cadventure-txt

And by simple, I mean *very* simple.
Currently, comments are not supported, = is not supported,
a single line of output (starting with >) is mandatory for every function,
and whitespace has to be very precise.

To play an adventure, you can compile the hs file and run it

    $ ghc --make adventure-text.hs
    $ ./adventure-text sample.story

Or you can run it interpreted:

    $ runghc adventure-text.hs sample.story

Or you can use the `loadAdventure` function in a REPL

    ghci> loadAdventure "sample.story"

Patches are welcome;
I may or may not beef up my Parsec skills
and clean up the code a bit.
