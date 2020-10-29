{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where
import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction  = do
  numerator <- decimal 
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty

  print $ parseFraction' badFraction
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad


testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork


someFunction :: Parser Integer
someFunction = do
  x <- integer
  _ <- eof
  return x


testAns :: IO ()
testAns = do
  let virtuousFraction' =
        parseString someFunction mempty
  print $ virtuousFraction' "123"
  

s :: String -> IO ()
s word = do
  let func' =
        parseString ( notChar 'a') mempty
  print $ func' word
  print $ func' word
  print $ func' word
  print $ func' word

