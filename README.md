# aplite

A parser and interpreter for a subset of APL.

Authors:
- Anirudh Konduru, akonduru@seas.upenn.edu
- Revathi Vijayaraghavan, revathiv@seas.upenn.edu

### Components

- The Languange definition is in src/Language.hs. This defines most of the types necessary to parse and execute APL code.
- The Parser is defined in src/Parser.hs. This uses MegaParsec to parse strings representing APL expressions into it's AST.
- Most computation in APL happens in the form of _functions_ (that convert values to other values) and _operators_, that convert functions to other functions. Some of the built-in functions and operators are defined and implemented in src/BuiltInFunctions.hs and src/BuiltInOperators.hs, respectively.
- The src/Eval.hs contains the functions necessary to run APL code, once it has been parsed into an APL Expression type.


### Dependencies
- MegaParsec: for all our parsing needs.
- parser-combinators: some helper combinators to help with parsing.
- mtl: For Monad transformers (StateT, ExceptT)
- math-functions: implementations of some commonly used functions
- gamma: implementation of the gamma function (for the APL _gamma_ function)
- split: helper functions for reshaping arrays (ex., `chunksOf`)
- prettyprinter: Library for pretty printing. Used for printing APL expressions and functions.
- containers: for Data.Map
