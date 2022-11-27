module Parser (module Parser) where

import Data.Void
import Language
  ( Dyadic (DSym),
    Expression (..),
    Monadic (MSym),
    Scalar (..),
    Value (..),
    aplOperators,
  )
import Text.Megaparsec
import Text.Megaparsec.Char (numberChar)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | parse a string with a given parser
parse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parse p = runParser p ""

-- Parse a positive or negative integer
number :: Parser Int
number = lexeme $ do
  sign <- option '+' (oneOf "+-")
  digits <- some numberChar
  let n = read digits
  return $ if sign == '-' then -n else n

char :: Char -> Parser Char
char c = lexeme (C.char c)

-- Parse a positive or negative float
float :: Parser Float
float = do
  sign <- option '+' (oneOf "+-")
  fstr <- (++) <$> option "0" (some numberChar) <*> ((:) <$> char '.' <*> some numberChar)
  let n = read fstr
  return $ if sign == '-' then -n else n

sc :: Parser ()
sc =
  L.space
    C.hspace1
    (L.skipLineComment "⍝")
    (L.skipBlockComment "⍝" "⍝")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

{-
parsec's try combinator is used to try a parser, if it fails, it backtracks to the previous state
this is necessary because Parsec by default only does LL(1) parsing, which means it only looks at the first character of a token
-}

scalarParser :: Parser Scalar
scalarParser = lexeme $ try (FloatVal <$> float) <|> (IntVal <$> number)

valueParser :: Parser Value
valueParser = naked $ lexeme $ try arrayParser <|> Scalar <$> scalarParser

arrayParser :: Parser Value
arrayParser = naked $ lexeme $ arrayOf <$> valuesParser

arrayOf :: [Value] -> Value
arrayOf vs = Array [length vs] vs

enclosed :: Parser a -> Parser a
enclosed = between (char '(') (char ')')

naked :: Parser a -> Parser a
naked p = try (enclosed p) <|> p

operator :: Parser Char
operator = lexeme $ oneOf aplOperators

-- parse a list of space separated values
valuesParser :: Parser [Value]
valuesParser = (:) <$> singleValueParser <*> some singleValueParser
  where
    singleValueParser = try (arrayOf <$> enclosed valuesParser) <|> try (Scalar <$> scalarParser) <|> try (Expression <$> enclosed expressionParser)

monadicParser :: Parser Expression
monadicParser = lexeme $ do
  op <- MSym <$> operator
  Monadic op <$> expressionParser

dyadicParser :: Parser Expression
dyadicParser = lexeme $ do
  left <- operandParser
  op <- DSym <$> operator
  Dyadic left op <$> expressionParser

operandParser :: Parser Expression
operandParser = try (enclosed expressionParser) <|> try (Value <$> valueParser)

expressionParser :: Parser Expression
expressionParser = lexeme $ try monadicParser <|> try dyadicParser <|> operandParser
