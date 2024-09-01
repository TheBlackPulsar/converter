module Converter (runConverter) where

import Parser (parseConversion)
import Numeric (showHex, showOct)
import Text.Printf (printf)

runConverter :: IO ()
runConverter = do
  putStrLn "Enter conversion (e.g., 'binary 1010 to decimal'): "
  input <- getLine
  case parseConversion input of
    Left err -> putStrLn $ "Error: " ++ err
    Right (from, value, to) -> case convert from value to of
      Just result -> putStrLn $ "Result: " ++ result
      Nothing     -> putStrLn "Invalid conversion."
  runConverter

convert :: String -> String -> String -> Maybe String
convert from value to = do
  decimalValue <- toDecimal from value
  convertFromDecimal to decimalValue

toDecimal :: String -> String -> Maybe Int
toDecimal "binary"     value = toBase 2 value
toDecimal "decimal"    value = readMaybe value
toDecimal "hexadecimal" value = toBase 16 value
toDecimal "octal"      value = toBase 8 value
toDecimal _            _     = Nothing

convertFromDecimal :: String -> Int -> Maybe String
convertFromDecimal "binary"     value = Just $ toBaseString 2 value
convertFromDecimal "decimal"    value = Just $ show value
convertFromDecimal "hexadecimal" value = Just $ showHex value ""
convertFromDecimal "octal"      value = Just $ showOct value ""
convertFromDecimal _            _     = Nothing

toBase :: Int -> String -> Maybe Int
toBase base value =
  case foldl (\acc x -> (+) <$> (* base) <$> acc <*> digitToInt x) (Just 0) value of
    Just n  -> Just n
    Nothing -> Nothing
  where
    digitToInt :: Char -> Maybe Int
    digitToInt c
      | '0' <= c && c <= '9' = Just (fromEnum c - fromEnum '0')
      | 'A' <= c && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
      | 'a' <= c && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
      | otherwise            = Nothing

toBaseString :: Int -> Int -> String
toBaseString base value
  | value < base = show value
  | otherwise    = toBaseString base (value `div` base) ++ show (value `mod` base)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing
