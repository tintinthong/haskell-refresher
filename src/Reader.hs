module Reader where
import Data.Char as Char

newtype Reader cfg a = Reader { runReader :: cfg -> a } 

data Reader2 cfg a = Reader2 { runReader2 :: cfg -> a } 
-- ask  :: Reader cfg cfg
-- asks :: (cfg -> a) -> Reader cfg oa

data Something = SomethingConstruct { x :: String} deriving (Show)
  
main = print "hi"



-- Our config data. We're very particular about certain
-- letters of the alphabet, see.
data ABConfig = ABConfig
  { don'tUseLetterE :: Bool
  , don'tUseLetterL :: Bool
  }

-- Uppercase the string, obeying current tests.
toUpperStr :: ABConfig -> String -> String
toUpperStr cfg str =
  filter passesFilters (fmap Char.toUpper str)
  where filters :: [Char -> Bool]
        filters =
          [ if don'tUseLetterE cfg then (/= 'E') else const True
          , if don'tUseLetterL cfg then (/= 'L') else const True
          ]

        passesFilters :: Char -> Bool
        passesFilters c = all (\f -> f c) filters

someConfig = ABConfig True True

fullName :: ABConfig -> String -> String -> String -> String
fullName cfg firstname lastname nickname =
  let
    a = toUpperStr cfg firstname
    b = toUpperStr cfg nickname
    c = toUpperStr cfg lastname
  in a ++ " " ++ "\"" ++ b ++ "\"" ++ " " ++  c


toUpperStr' :: String -> Reader ABConfig String
toUpperStr' str = Reader (\cfg ->
  let filters :: [Char -> Bool]
      filters = [ if don'tUseLetterE cfg then (/= 'E') else const True
                , if don'tUseLetterL cfg then (/= 'L') else const True
                ]
      passesFilters :: Char -> Bool
      passesFilters c = all (\f -> f c) filters
  in filter passesFilters (fmap Char.toUpper str))


instance Functor (Reader cfg) where
  fmap f (Reader g) = Reader (fmap f g) 

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

instance Functor (Either' a) where
    fmap f (Left' x) = Left' x
    fmap f (Right' x) = Right' (f x)

instance Applicative (Reader cfg) where
  pure x =  Reader $ \_ -> x 
  -- (Reader (const x))
  (Reader cfgtoAtoB) <*> (Reader cfgtoA) = Reader (\cfg -> cfgtoAtoB cfg $ cfgtoA cfg )
  -- (Reader cfgg f) <*> (Reader cfgg x) = Reader cfgg (f x) 
  -- <*> f(a->b)-> f(a) -> f(b)

instance Monad (Reader cfg) where
  return x = Reader $ const x
  (Reader cfgtoA ) >>= nextFn = Reader (\cfg -> runReader (nextFn (cfgtoA cfg)) cfg)
  -- cfgToA : cfg -> a
  -- cfgToA cfg : a  
  -- nextFn : a -> Reader cfg b 
  -- nextFn cfgToA cfg : Reader cfg b  
  -- runReader (nextFn cfgToA cfg) cfg : Reader cfg b
-- >>= m a -> (a -> m b) -> m b
-- >>= Reader cfg a -> (a -> Reader cfg b) -> Reader cfg b 
  

ask :: Reader cfg cfg 
ask = Reader id


asks :: (cfg-> a) -> Reader cfg a
asks lambda = Reader lambda
  

asks':: (cfg -> a) -> Reader cfg a
asks' f = do
  cfg <- ask
  return (f cfg)
  

toUpperStr'' :: String -> Reader ABConfig String
toUpperStr'' str = do
  cfg <- ask
  let filters :: [Char -> Bool]
      filters = [ if don'tUseLetterE cfg then (/= 'E') else const True
                , if don'tUseLetterL cfg then (/= 'L') else const True
                ]
      passesFilters :: Char -> Bool
      passesFilters c = all (\f -> f c) filters
  pure (filter passesFilters (fmap Char.toUpper str))


welcomeMessage :: String -> String -> Reader ABConfig String
welcomeMessage motd username = do
  cfg <- ask
  upperMOTD <- toUpperStr'' motd
  upperUsername <- toUpperStr'' username
  pure (
    "Welcome, " ++  upperUsername ++ "Message of the day " ++ upperMOTD
       )

local :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
local f (Reader g) = Reader ( g . f)

  -- do
  -- cfg <- ask 
  -- x <- runReader gReader (f cfg) 
  -- Reader (\cfg-> x)



  -- toUpperStr motd >>= (\upperMOTD ->
  --   toUpperStr username >>= (\upperUsername ->
  --     Reader (\_ ->
  --       "Welcome, " ++
  --       upperUsername ++
  --       "! Message of the day: " ++
  --       upperMOTD)))

-- toUpperStr'' :: String -> Reader ABConfig String
-- toUpperStr'' str = Reader (\cfg ->
--   let filters :: [Char -> Bool]
--       filters = [ if don'tUseLetterE cfg then (/= 'E') else const True
--                 , if don'tUseLetterL cfg then (/= 'L') else const True
--                 ]
--       passesFilters :: Char -> Bool
--       passesFilters c = all (\f -> f c) filters
--   in filter passesFilters (fmap Char.toUpper str))
