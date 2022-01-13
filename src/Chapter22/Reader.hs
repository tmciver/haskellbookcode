module Chapter22.Reader where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (\r -> f (ra r))

-- exercise 1
ask :: Reader r r
ask = Reader id

-- exercise: reading comprehension
myLiftA2 :: Applicative f
         => (a -> b -> c)
         -> f a
         -> f b
         -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader $ \r -> f r
