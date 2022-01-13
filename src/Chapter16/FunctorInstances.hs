module Chapter16.FunctorInstances where

newtype Identity a = Indentity a
data Pair a = Pair a a
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four a b c d = Four a b c d
data Four' a b = Four' a a a b

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
