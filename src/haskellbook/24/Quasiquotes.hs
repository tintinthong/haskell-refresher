
{-# LANGUAGE QuasiQuotes #-}

module Quasimodo where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

type NumberOrString =
  Either Integer String

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >>
  (Left <$> integer)
  <|> (Right <$> some letter)

main = do
  let p f i = parseString f mempty i
  print $ p (some parseNos) eitherOr


-- somefunc (x:xs)  
--   | x == 1 = x
--   | length xs == 1 = x
