module Lambda.Functor (
  MyMaybe(..),
  MyReader(..), runMyReader) 
  where 
    
data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)
instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

newtype MyReader a b = MyReader { unwrap :: a -> b }
instance Functor (MyReader c) where 
  fmap f (MyReader g) = MyReader (f.g)

runMyReader :: MyReader a b -> a -> b
runMyReader = unwrap