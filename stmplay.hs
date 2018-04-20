

module Main where

import Control.Concurrent.STM
import System.IO
import Data.IORef
import Control.Concurrent


incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)



main :: IO ()
main = do
    tid <- forkIO (hPutStr stdout "Hello")
    var <- newIORef 42
    incRef var 
    val <- readIORef var
    hPutStr stdout (show val)
    hPutStr stdout "Hey There\n"


