#hs-adventure-txt
**Simple text adventure engine in Haskell**

This is a simple implementation of the Cadventure-txt format:
http://github.com/tech-no-crat/Cadventure-txt

Note: I've updated the code to be much more liberal
about whitespace, and all features of the spec are now implemented.
However, I didn't handle one thing: leading whitespace for option lists.
'-' must be the first character of each option list line.

To play an adventure, you can compile the hs file and run it

    $ ghc --make adventure-text.hs
    $ ./adventure-text sample.story

Or you can run it interpreted:

    $ runghc adventure-text.hs sample.story

Or you can use the `loadAdventure` function in a REPL

    ghci> loadAdventure "sample.story"

Patches are welcome. :)
