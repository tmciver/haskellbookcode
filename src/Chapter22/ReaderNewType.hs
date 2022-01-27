{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter22.ReaderNewType where


main :: IO ()
main = do
  env <- loadEnv
  let str = runReader func1 env
  print str

data Environment = Environment
  { param1 :: String
  , param2 :: String
  , param3 :: String
  } deriving Show

newtype Reader r a = Reader { runReader :: r -> a }
  deriving (Functor, Applicative, Monad)

ask :: Reader r r
ask = Reader id
-- ask = Reader (\r -> r)

loadEnv :: IO Environment
loadEnv = pure (Environment "portland" "functional" "programming")

func1 :: Reader Environment String
func1 = fmap f func2
  where f i = "Result: " ++ show i


func2 :: Reader Environment Int
func2 = fmap f func3
  where f fl = 2 + floor fl


func3 :: Reader Environment Float
func3 = do
  env <- ask
  let l1 = length (param1 env)
      l2 = length (param2 env) * 2
      l3 = length (param3 env) * 3
      flt = (fromIntegral $ l1 + l2 + l3) * 2.1
  return flt
