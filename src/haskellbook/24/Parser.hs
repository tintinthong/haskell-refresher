module Parser where
-- import Text.Parser.Char        (char, digit)
-- import Text.Parser.Combinators (count, some, try)
import Control.Applicative     
import Text.Trifecta
-- (Parser, parseString)

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' 

one' = one >> stop

one'' :: Parser ()
one'' = one >> eof 

oneTwo'' :: Parser ()
oneTwo'' = oneTwo >> eof 

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' 


testParse :: Parser Char -> IO ()
testParse p = print $  parseString p  mempty "123"

testParse' :: Parser () -> IO ()
testParse' p = print $  parseString p  mempty "12"

-- testParse'' :: Parser () -> IO ()
-- testParse'' = do
--   x <- testParse' one 

oneTwo' = oneTwo >> stop 

choiceParse = choice [oneTwo, one]

allThree :: Parser String 
allThree = do
  n <- (show <$> integer)
  _ <- eof 
  return n 

-- main :: IO ()
-- main = do
--   -- parseTest one "1x"   -- this will succeed parsing
--   parseTest oneTwo "12"
--   -- parseTest one "x1"   -- this will fail parsing

-- print string on new line 
pNL s =putStrLn ('\n' : s)
-- priority = option 0 (digitToInt <$> digit)

-- string' :: CharParsing m => String -> m String
-- string' | (x:xs) = (char x) <|> (string' xs)

string' :: String -> Parser String
string' str = go str mempty
  where
    go (x:xs) parsed = do
      x' <- char x
      go xs (parsed ++ [x'])
    go [] parsed     = return parsed

main = do
  -- pNL "stop:"
  -- testParse stop
  pNL "one:"
  testParse one
  -- pNL "one':"
  -- testParse one'
  -- pNL "oneTwo:"
  -- testParse oneTwo
  -- pNL "oneTwo':"
  -- testParse oneTwo'
  -- pNL "one'':"
  -- testParse' one''
  -- pNL "oneTwo''':"
  -- testParse' oneTwo''
  -- pNL "choiceParser:"
  -- testParse choiceParse
  print $ parseString (string "123") mempty "123"
  print $ parseString (string' "12") mempty "12"
  -- pNL "p123':"
  -- testParse p123 


-- p123 ::  Parser Char
-- p123 string = one

