{-# LANGUAGE InstanceSigs #-}

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

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r ->
    let ab = rab r
        a = ra r
    in ab a

-- exercise: Reader Monad
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r ->
    let a = ra r
        Reader rb = aRb a
    in rb r
