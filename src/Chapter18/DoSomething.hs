module Chapter18.DoSomething ( doSomething
                             , doSomething'
                             , zed
                             , zed'
                             ) where

-- Code deomonstrating that certain kinds of monadic code cannot be converted
-- to the Applicative style. This is the case (I think) if the final bind
-- operation (or the last line when using do notation) does not contain a call
-- to `return`. Lack of a call to `return` means that the final expression
-- returns monadic structure which, when using Applicative style, causes
-- nested monadic structure requiring the use of `join`.

f :: Maybe Integer
f = Just 1

g :: Maybe String
g = Just "1"

h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do
  a <- f
  b <- g
  c <- h
  return (zed a b c)

zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a, b, c)

doSomething' = do
  a <- f
  b <- g
  c <- h
  zed' a b c

-- doSomething'' :: Maybe (Integer, String, Integer)
-- doSomething'' = pure zed' <*> f <*> g <*> h

doSomething''' :: Maybe (Integer, String, Integer)
doSomething''' = pure zed <*> f <*> g <*> h
