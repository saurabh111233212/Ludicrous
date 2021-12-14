# Ludicrous
David (`xudavid`) and Saurabh (`surb`)

## Overview
A purely functional, terminal based text editor written in Haskell for the language Lu, a reduced form of the scripting language Lua. We have supported basic text editing functions such as autocorrect, syntax highlighting, and autoformatting.

## Module organization
Below are the major components. We suggest you read the modules in the following order. The first three are "libraries" that we created to aid in our creating a functional text editor, and the fourth module `GUI` contains all of the code that defines the user-facing terminal interface.

1. `AutoCorrect`: A memoized implementation of an autocorrect feature based off of the Levenshtein distance.

2. `ColorMapper`: A line by line syntax highlighter for Lu using parser combinators in the `parsec` library.

3. `Formatter`: A pretty printing formatting module to aid in formatting parsable lu text

4. `GUI`: a GUI module implementing a text editor using the `Brick` declarative interface library.

5. `LuParser.hs` A parser for the Lu Language.

6. `LuSyntax.hs` Defines a pure representation of the syntax for the Lu language.

7. `Parser.hs` A general parsing library. 

## Dependencies
We have additional library dependencies. Most notably, other than standard libraries, we have included libraries such as `Brick`, a declarative terminal UI library, `Parsec`, a parsing library, and `Monad-memo`, a memoization monad written in Haskell.

Additionally, we used other libraries such as `vty` for graphics, `microlens` for Lens operations, as well as `pretty` for pretty printing, `text` for the `Text` datastructure, and `vector` for vector operations.

## Building and testing

This project compiles with `stack build`. 

We can test with `stack test`. This will run our suite of HUnit and QuickCheck tests.

## User Interface

You can run the main executable with either 
```
stack run [lu file name] [file containing possible words]
```
or the equivalent formulation given the binary `ludicrous`:
```
ludicrous [lu filename] [file containing possible words]
```

This should open the text editor. When within the text editor, you are free to type (excluding using visual mode). Additionally, `^F` formats, `^T` replaces the current word with the autocorrect suggestion, and `^Z` undos. You may exit the text editor using `Esc`, which saves and formats your file.
