

module Chapter22.ReaderTypeAlias2 where


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

type Reader r a = r -> a

runReader :: Reader r a -> r -> a
runReader f x = f x

ask :: Reader r r
ask = \r -> r
-- ask = id

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
