module Chapter15.Optional where

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> other = other
  other <> Nada = other
  Only a <> Only b = Only $ a <> b

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  Nada `mappend` other = other
  other `mappend` Nada = other
  Only a `mappend` Only b = Only $ a `mappend` b
