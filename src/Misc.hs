module Misc (mkOnce) where

import Control.Monad
import qualified Control.Concurrent as C

import Data.IORef

startProcess :: IO (IO ())
startProcess = do
    i <- C.forkIO (forever (putStrLn "your PC is stoned"))
    return (C.killThread i)

-- This code was given to me by someone on #haskell-beginners (don't remember
-- who now :() as another demonstration of a function that returns IO (IO a).
mkOnce :: IO a -> IO (IO a)
mkOnce x =
  do r <- newIORef Nothing
     return $ do m <- readIORef r
                 case m of
                   Nothing -> do v <- x
                                 writeIORef r (Just v)
                                 return v
                   Just v -> return v

main = do
    killer <- startProcess
    C.threadDelay 2000000
    killer
