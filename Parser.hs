module Parser (parseConversion) where

import Text.ParserCombinators.Parsec

data Conversion = Conversion String String String

parseConversion :: String -> Either String (String, String, String)
parseConversion input =
  case parse conversionParser "conversion" input of
    Left err -> Left $ show err
    Right (from, value, to) -> Right (from, value, to)

conversionParser :: Parser (String, String, String)
conversionParser = do
  from <- many1 letter
  spaces
  value <- many1 (digit <|> letter)
  spaces
  _ <- string "to"
  spaces
  to <- many1 letter
  return (from, value, to)
