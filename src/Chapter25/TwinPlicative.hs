{-# LANGUAGE InstanceSigs #-}

module Chapter25.TwinPlicative where

newtype Compose f g a = Compose { getCompose :: f (g a) }
                      deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
--  (Compose f) <*> (Compose a) = Compose (fmap (\gab -> fmap (\ga -> gab <*> ga) a) f)
--  (Compose f) <*> (Compose a) = Compose ((fmap . fmap) (\g -> (fmap . fmap) g a) f)
-- came to the following working solution after some helpful hints from someone
-- (can't rememer who) on IRC.  Good thing too; I never would have gotten this
-- on my own. :(
  (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a
