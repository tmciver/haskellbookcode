module Chapter15.Optional where

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Only mempty
  Nada `mappend` other = other
  other `mappend` Nada = other
  Only a `mappend` Only b = Only $ a `mappend` b
