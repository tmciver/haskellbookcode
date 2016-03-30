module Misc () where

import Control.Monad
import qualified Control.Concurrent as C

startProcess :: IO (IO ())
startProcess = do
    i <- C.forkIO (forever (putStrLn "your PC is stoned"))
    return (C.killThread i)

main = do
    killer <- startProcess
    C.threadDelay 2000000
    killer
