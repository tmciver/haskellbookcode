module Chapter17.ZipList where

-- import Test.QuickCheck.Checkers (EqProp, (=-=), eq)

data List a = Nil | Cons a (List a)
            deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x l) = Cons x (take' (n-1) l)

concat' :: List a -> List a -> List a
concat' xs Nil = xs
concat' Nil ys = ys
concat' (Cons x xs) ys = Cons x (concat' xs ys)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = concat' (fmap f xs) (fs <*> xs)

newtype ZipList' a = ZipList' (List a)
                   deriving (Eq, Show)

-- instance Eq a => EqProp (ZipList' a) where
--   xs =-= ys = xs' `eq` ys'
--     where xs' = let (ZipList' l) = xs
--                 in take' 3000 l
--           ys' = let (ZipList' l) = ys
--                 in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) = ZipList' (Cons (f x) theRest)
    where theRest = let ZipList' l = ZipList' fs <*> ZipList' xs in l
