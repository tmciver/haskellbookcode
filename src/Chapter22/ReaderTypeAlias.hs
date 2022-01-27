

module Chapter22.ReaderTypeAlias where


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



loadEnv :: IO Environment
loadEnv = pure (Environment "portland" "functional" "programming")

func1 :: Reader Environment String
func1 = \env -> let res = runReader func2 env
                    str = "Result: " ++ (show res)
                in str

func2 :: Reader Environment Int
func2 = \env -> let res = runReader func3 env
                    i = 2 + floor res
                in i

func3 :: Reader Environment Float
func3 = \env -> let l1 = length (param1 env)
                    l2 = length (param2 env) * 2
                    l3 = length (param3 env) * 3
                    flt = (fromIntegral $ l1 + l2 + l3) * 2.1
                in flt
