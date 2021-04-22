module Chapter12.Exercises where

-----------------------------------
-- Section: Small library for Maybe
-----------------------------------
mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe def f maybe' = case maybe' of
  Nothing -> def
  Just x -> f x

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
  where f :: Maybe a -> [a] -> [a]
        f Nothing xs = xs
        f (Just x) xs = x:xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f Nothing _ = Nothing
        f (Just x) maybeXs = (x:) <$> maybeXs

