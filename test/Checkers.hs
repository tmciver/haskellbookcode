import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Chapter18.EighteenPointSeven
import Chapter15.Optional

--instance EqProp (Nope a) where (=-=) = eq
instance (Eq a) => EqProp (Optional a) where (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

-- I still need to figure out exactly why I needed to specify a 3-tuple here . . .
main = do
--  quickBatch (monad (NopeDotJpg :: Nope (String, Char, Char)))
  quickBatch (monoid (undefined :: Optional String))
