module Chapter22.Reader where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (\r -> f (ra r))

ask :: Reader r r
ask = Reader id
