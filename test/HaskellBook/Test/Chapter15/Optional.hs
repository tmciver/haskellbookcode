module HaskellBook.Test.Chapter15.Optional where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Chapter15.Optional

instance Arbitrary (Optional (Sum Int)) where
  arbitrary = choose [Nada, liftM Only arbitrary]
