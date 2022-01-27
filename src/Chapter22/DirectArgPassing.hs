

module Chapter22.DirectArgPassing where

-- Taken from https://mmhaskell.com/monads/reader-writer
main :: IO ()
main = do
  env <- loadEnv
  let str = func1 env
  print str

data Environment = Environment
  { param1 :: String
  , param2 :: String
  , param3 :: String
  } deriving Show








loadEnv :: IO Environment
loadEnv = return (Environment "portland" "functional" "programming")

func1 :: Environment -> String
func1 env = "Result: " ++ (show (func2 env))



func2 :: Environment -> Int
func2 env = 2 + floor (func3 env)



func3 :: Environment -> Float
func3 env = (fromIntegral $ l1 + l2 + l3) * 2.1
  where
    l1 = length (param1 env)
    l2 = length (param2 env) * 2
    l3 = length (param3 env) * 3
