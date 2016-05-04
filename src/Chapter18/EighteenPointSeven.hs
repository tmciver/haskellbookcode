module Chapter18.EighteenPointSeven
       ( Nope(..) ) where

data Nope a = NopeDotJpg
            deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

  pure _ = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= f = NopeDotJpg
