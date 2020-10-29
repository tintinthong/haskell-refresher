module Monad where

-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/13-the-list-monad

import Data.Char

data List a = Nil | Cons a (List a)

join :: List (List a) -> List a
join Nil = Nil
join (Cons xs xss) =  cat xs (join xss)

cat :: List a -> List a -> List a
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

l1 = Cons 1 (Cons 2 Nil)
l2 = Cons 3 Nil

-- main = print $ join $ Cons l1 (Cons l2 Nil)


instance (Show a) => Show (List a) where
  show Nil = ""
  show (Cons x xs) = show x  ++ ", " ++ show xs

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs) 
  
instance Monad List where
  return x = Cons x Nil
  xs >>= k = join $ fmap k xs
-- ma -> (a -> mb) -> mb

instance Semigroup (List a) where
  (<>) = cat

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> Cons x xs = Cons (f x) ((fmap f xs) <> (fs <*> xs))
  -- (<*>) :: f (a -> b) -> f a -> f b 


neighbors :: (Num a) => a -> a -> List a
neighbors x dx = Cons (x - dx) (Cons x (Cons (x + dx) Nil))
-- [1,2,3] => neighbors 0 1

test = do
    -- x <- neighbors 0 100 
    x <- Cons 1 (Cons 2 (Cons 3 Nil))
    y <- neighbors x 1
    return y 


-- main = print $ test

join' :: Maybe ( Maybe a ) -> Maybe a
join' Nothing = Nothing
join' (Just ma) = ma
-- join' mma = mma >>= id


test1, test2, test3 :: Maybe (Maybe String)
test1 = Nothing
test2 = Just Nothing
test3 = Just (Just "a little something")

-- main = do
--     print $ join' test1
--     print $ join' test2
--     print $ join' test3


listBind :: [ a ] -> (a ->  [ b ]) ->  [ b ]
listBind a f = (fmap f a)  >>= id

listReturn :: a -> [ a ]
listReturn x = [x]

neighbors' x = [x - 1, x, x + 1]

main = do
    print $ listBind [10, 20, 30] neighbors'
    print $ listBind "string" (listReturn . ord)


-- (<=<) :: (Monad m)=> (b->mc) -> (a->mb)-> (a->mc)
-- g <=< f = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
g <=< f = \x -> f x >>= g

f x = [x, x + 1]
g x = [x * x]

test' = g <=< f

main'' = print $ test' 1
 
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- f >=> g = g <=< f 

-- f' x = [x, x + 1]
-- g' x = [x * x]

-- test'' = f >=> g

-- main''' = print $ test'' 7

(>=>) :: (a -> [ b]) -> (b -> [ c ]) -> (a -> [ c ])
-- f >=> g = \x -> f x  >>= g
f >=> g = \x -> concat (map g (f x))

modCase c = [toLower c, toUpper c]
camelize = modCase >=> modCase

main'''' = print $ fmap camelize "Hump"

squares lst = do
    x <- lst
    return (x * x)

squares'' lst = lst >>= \x -> return (x*x)
  

squares' lst = fmap (\x-> round(x**2)) lst

squares''' lst = 
    concat $ fmap k lst
  where
    k = \x -> [x * x]

squareSquare lst = do
  x <- lst
  y <- squares [x*x]
  return y
  


main''''' = print $ squares'' [1, 2, 3]

-- main'''''' = print $ squareOfSquares [1, 2, 3]

pairs l1 l2 = do
    x <- l1
    y <- l2
    return (x, y)
-- [1,2,3] >>= \x -> "abc" >>= \y -> [(x,y)]

mainz = print $ pairs [1, 2, 3] "abc"


-- data Suit = Clubs | Hearts | Diamonds | Spades deriving (Show ,Enum)

-- data Rank = Rank Int 

-- type Card = (Rank, Suit) 

-- instance Show Rank where 
--   show rankN = show rankN

-- deck = [ ( rank, suit ) | rank <- [ 1 .. 13], suit<- [Clubs .. Spades]  ]

-- data Rankie a = Ex a | Ex2 a 

-- instance (Show a) => Show (Rankie a) where
--   show (Ex n) = show n
--   show (Ex2 n) = show 50 


data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum)

data Rank = Rank Int

instance Show Rank where
    show (Rank 1)  = "Ace"
    show (Rank 11) = "Jack"
    show (Rank 12) = "Queen"
    show (Rank 13) = "King"
    show (Rank i)  = show i

deck = [(Rank r, s) | s <- [Club .. Spade]
                    , r <- [1..13]]

-- mainzz = print deck

-- instance (Show a) => Show (List a) where
--   show Nil = ""
--   show (Cons x xs) = show x  ++ ", " ++ show xs
