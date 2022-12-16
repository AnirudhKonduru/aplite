{-# LANGUAGE LambdaCase #-}

module MkParser (module MkParser) where

import qualified BuiltInFunctions as Bif
import qualified BuiltInOperators as Bio
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Identity
import Control.Monad.State
import Data.Foldable (find)
import Data.Map hiding (filter)
import qualified Data.Map as Map hiding (filter)
import Data.Void
import Language
  ( DyadicOperator (..),
    Expression (..),
    FunctionExpression (BuiltInFunction, DOpF, MOpF),
    MonadicOperator (..),
    Scalar (..),
    Value (..),
    aplDyadicOperators,
    aplFunctions,
    aplMonadicOperators,
  )
import Text.Megaparsec
  ( MonadParsec (eof, try),
    ParseErrorBundle,
    ParsecT,
    between,
    many,
    oneOf,
    option,
    parseTest,
    runParser,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (numberChar)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Env = Map String Int

emptyEnv :: Env
emptyEnv = Map.empty

-- our Parser type is a StateT transformer over a ParsecT transformer
-- The StateT transformer keeps track of the current environment (This is not used yet)
type Parser a = StateT Env (ParsecT Void String Identity) a

-- | parse a string with a given parser and print it out
parseTest' :: (Show a) => Parser a -> String -> IO ()
parseTest' p = Text.Megaparsec.parseTest $ evalStateT p Map.empty

-- | parse a string with a given parser
parse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parse p = runParser (evalStateT p Map.empty) ""

-- | parse and pretty print
parse' :: Parser a -> String -> a
parse' p e = case runParser (evalStateT p Map.empty) "" e of
  Left err -> error (show err)
  Right x -> x

-- Parse a positive or negative integer
number :: Parser Int
number =
  lexeme $
    -- note that APL uses the high minus '¯' as a negative sign, not '-'
    try (negate . read <$> (C.char '¯' *> number'))
      <|> try (read <$> number')
  where
    number' :: Parser String
    number' = some numberChar

char :: Char -> Parser Char
char c = lexeme (C.char c)

-- Parse a positive or negative float
float :: Parser Float
float =
  lexeme $
    -- note that APL uses the high minus '¯' as a negative sign, not '-'
    try (negate . read <$> (C.char '¯' *> float'))
      <|> try (read <$> float')
  where
    float' :: Parser String
    float' = (++) <$> option "0" (some numberChar) <*> ((:) <$> char '.' <*> some numberChar)

-- | Parse using a parser and consume any trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space C.hspace1 (L.skipLineComment "⍝") (L.skipBlockComment "⍝" "⍝")

{-
parsec's try combinator is used to try a parser, if it fails, it backtracks to the previous state
this is necessary because Parsec by default only does LL(1) parsing, which means it only looks at the first character of a token
-}

-- Parsers for APL types and expressions

scalarParser :: Parser Scalar
scalarParser =
  lexeme $
    try (Number <$> float)
      <|> try (Number . toEnum <$> number)

valueParser :: Parser Value
valueParser = lexeme $ try arrayParser <|> try (Scalar <$> scalarParser)

arrayParser :: Parser Value
arrayParser = maybeEnclosed $ lexeme $ try (arrayOf <$> valuesParser)

arrayOf :: [Value] -> Value
arrayOf vs = Array [length vs] vs

-- | Parse an expression that is enclosed in parentheses
enclosed :: Parser a -> Parser a
enclosed = lexeme . between (char '(') (char ')')

maybeEnclosed :: Parser a -> Parser a
maybeEnclosed p = lexeme $ try (enclosed p) <|> p

builtInFunctionChar :: Parser Char
builtInFunctionChar = lexeme $ oneOf aplFunctions

builtInMonadicOperatorChar :: Parser Char
builtInMonadicOperatorChar = lexeme $ oneOf aplMonadicOperators

builtInDyadicOperatorChar :: Parser Char
builtInDyadicOperatorChar = lexeme $ oneOf aplDyadicOperators

-- parse a list of space separated values
valuesParser :: Parser [Value]
valuesParser = try $ (:) <$> singleValueParser <*> some singleValueParser
  where
    singleValueParser :: Parser Value
    singleValueParser = try (enclosed arrayParser) <|> try (Scalar <$> scalarParser)

-- dyadicParser :: Parser Expression
-- dyadicParser = lexeme $ chainr1 expressionParser dyadicExpParser

variableParser :: Parser String
variableParser = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

-- | A term is anything that can show up as arguments
-- to a function (or function expression)
termParser :: Parser Expression
termParser =
  lexeme $
    -- \| trying to read values directly as an array/scalar
    -- \| instead of as an expression (EArray) prevents it
    -- \| from parsing mixed arrays as a single array
    -- try (EValue <$> valueParser)
    try (EArray <$> valuesParser')
      <|> try (EVariable <$> variableParser)
      <|> try (enclosed expressionParser)
      <|> try expressionParser

-- | Parse a function expression.
-- Here the terms are functions, and they can be separated by
-- operators (dyadic or monadic, infix or postfix).
functionExpressionParser :: Parser FunctionExpression
functionExpressionParser =
  makeExprParser
    builtInFunctionParser
    [ [InfixL (DOpF <$> builtInDyadicOperatorParser)],
      [Postfix (MOpF <$> builtInMonadicOperatorParser)]
    ]


-- | Parse an expression.
-- Here the terms are values, and they can be separated by
-- functions expressions (which can be built in functions or operator chains)
expressionParser :: Parser Expression
expressionParser =
  makeExprParser
    termParser
    [ [ -- dyadic functions
        InfixR (EDyadic <$> functionExpressionParser)
      ],
      [ -- monadic functions
        Prefix (EMonadic <$> functionExpressionParser)
      ]
    ]

-- | Parse a built in function, which is a single character
builtInFunctionParser :: Parser FunctionExpression
builtInFunctionParser = do
  c <- builtInFunctionChar
  -- find the function in builtInMonadicFunctions with the same symbol
  let maybeFunc = find (\case (BuiltInFunction sym _) -> sym == [c]; _ -> False) Bif.builtInFunctions
  case maybeFunc of
    Just builtInFunc -> return builtInFunc
    Nothing -> error "builtInFunctionParser: not found"

-- | Parse a built in monadic operator, which is a single character
builtInMonadicOperatorParser :: Parser MonadicOperator
builtInMonadicOperatorParser = lexeme $ do
  c <- builtInMonadicOperatorChar
  return $ uncurry (MonadicOperator [c]) $ case c of
    '/' -> ("reduce", Bio.reduce)
    '\\' -> ("scan", Bio.scan)
    _ -> error "builtInOperatorParser: impossible"

-- | Parse a built in dyadic operator, which is a single character
builtInDyadicOperatorParser :: Parser DyadicOperator
builtInDyadicOperatorParser = lexeme $ do
  c <- builtInDyadicOperatorChar
  return $ uncurry (DyadicOperator [c]) $ case c of
    '.' -> ("dot", Bio.product)
    '\\' -> ("nm", Bio.product)
    _ -> error "builtInOperatorParser: impossible"

-- parse a list of space separated values
valuesParser' :: Parser [Expression]
valuesParser' = try $ some singleValueParser
  where
    singleValueParser = try (EArray <$> enclosed valuesParser') <|> try (EValue . Scalar <$> scalarParser) <|> try (enclosed expressionParser)


-- | Parse a complete expression, including any trailing whitespace
complete :: Parser a -> Parser a
complete p = lexeme $ p <* eof
