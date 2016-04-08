{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 ( Goats(..)
                 , tooMany
                 , Product(..)
                 , Sum(..)
                 , RecordProduct
                 , Name
                 , Age
                 , LovesMud
                 , PoundsOfWool
                 , CowInfo(..)
                 , PigInfo(..)
                 , SheepInfo(..)
                 , Animal(..)
                 , Animal'
                 ) where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, Num, TooMany)

-- this will do the same thing as the
-- Int instance, but we still have to
-- define it separately
--instance TooMany Goats where
--  tooMany (Goats n) = tooMany n

-- section 11.7, exercise 1
instance TooMany (Int, String) where
  tooMany (n, s) = n + (length s) > 42

-- section 11.7, exercise 2
instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 42

-- section 11.7, exercise 3
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)

data Product a b = Product a b
                 deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a
                                       , psecond :: b }
                       deriving (Eq, Show)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
             deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud
             deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool
               deriving (Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
