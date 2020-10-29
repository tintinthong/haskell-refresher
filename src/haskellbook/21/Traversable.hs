{-# LANGUAGE InstanceSigs #-} -- enable type signature within instance
-- 21.12 Chapter Exercises (pg 839)

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap :: (a-> b)-> Identity a -> Identity b 
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldl = undefined

instance Traversable Identity where
  -- traverse :: (Applicative f, Traversable t) => (a-> f b) -> t a -> t (f b)
  traverse f (Identity a)= Identity  <$> (f a)


-- Constant 

newtype Constant a b = Constant { getConstant :: a }

main = do
  print "hi"
