module Chapter15.Optional where

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = undefined
  mappend = undefined
