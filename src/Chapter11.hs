{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 ( Goats(..)
                 , tooMany
                 ) where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

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
