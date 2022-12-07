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
number =
  lexeme $ do
    -- note that APL uses the high minus '¯' as a negative sign, not '-'
    numAbs <- read <$> number'
    maybeSign <- optional $ C.char '¯'
    case maybeSign of
      Just _ -> return $ negate numAbs
      Nothing -> return numAbs
  where
    number' :: Parser [Char]
    number' = reverse <$> some numberChar

char :: Char -> Parser Char
char c = lexeme (C.char c)

-- Parse a positive or negative float
float :: Parser Float
float = lexeme $ do 
    -- note that APL uses the high minus '¯' as a negative sign, not '-'
      -- fractional part
      frac <- reverse <$> some numberChar
      -- decimal point
      _ <- char '.'
      -- integer part
      whole <- reverse <$> option "0" (some numberChar)
      let absFloat = read $ whole ++ "." ++ frac
      maybeSign <- optional $ C.char '¯'
      case maybeSign of
        Just _ -> return $ negate absFloat
        Nothing -> return absFloat

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
scalarParser =
  lexeme $
    try (FloatVal <$> float)
      <|> try (IntVal <$> number)

valueParser :: Parser Value
valueParser = lexeme $ try arrayParser <|> try (Scalar <$> scalarParser)

arrayParser :: Parser Value
arrayParser = naked $ lexeme $ try (arrayOf <$> valuesParser)

arrayOf :: [Value] -> Value
arrayOf vs = Array [length vs] vs

enclosed :: Parser a -> Parser a
enclosed = lexeme . between (char '(') (char ')')

naked :: Parser a -> Parser a
naked p = lexeme $ try (enclosed p) <|> p

operator :: Parser Char
operator = lexeme $ oneOf aplOperators

-- parse a list of space separated values
valuesParser :: Parser [Value]
valuesParser = try $ (:) <$> singleValueParser <*> some singleValueParser
  where
    singleValueParser :: Parser Value
    singleValueParser = try (enclosed arrayParser) <|> try (Scalar <$> scalarParser)

monadicParser :: Parser Expression
monadicParser = lexeme $ do
  op <- MSym <$> operator
  EMonadic op <$> expressionParser

dyadicParser :: Parser Expression
dyadicParser = lexeme $ do
  left <- expressionParser
  op <- DSym <$> operator
  EDyadic left op <$> expressionParser

variableParser :: Parser String
variableParser = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

-- operandParser :: Parser Expression
-- operandParser =
--     try (EValue <$> valueParser)
--   <|> try (enclosed expressionParser)
--     <|> try (EArray <$> many expressionParser)
--     <|> try (EVariable <$> variableParser)

eValueParser :: Parser Expression
eValueParser =
  lexeme $
    try
      ( EArray
          <$> many
            ( try (enclosed expressionParser)
                <|> try (EArray <$> many expressionParser)
                <|> try (EValue . Scalar <$> scalarParser)
                <|> try (EVariable <$> variableParser)
            )
      )

-- <|> try (EValue <$> arrayParser)
-- try (EArray <$> many expressionParser)
--  <|> try (EValue <$> valueParser)
--  <|> try (EArray <$> many expressionParser)


-- parse a list of space separated values
valuesParser' :: Parser [Expression]
valuesParser' = try $ (:) <$> singleValueParser <*> some singleValueParser
  where
    singleValueParser = try (EArray <$> enclosed valuesParser') <|> try (EValue . Scalar <$> scalarParser) <|> try (enclosed expressionParser)

expressionParser :: Parser Expression
expressionParser =
  lexeme $
    try monadicParser
      <|> try dyadicParser
      <|> try (enclosed expressionParser)
      <|> try (EArray <$> valuesParser')
      <|> try (EValue . Scalar <$> scalarParser)

complete :: Parser a -> Parser a
complete p = lexeme $ p <* eof

-- >>> parseTest monadicExpression "1"
-- ProgressCancelledException
-- ProgressCancelledException
