import Control.Concurrent
import Control.Monad
import System.IO

main = forever $ do
	s <- getLine
	forkIO $ setReminder s 
	{-
	hSetBuffering stdout NoBuffering
	forkIO $ replicateM_ 10000 $ putChar 'A'
	replicateM_ 10000 $ putChar 'B'
	-}
	m <- newEmptyMVar
	forkIO $ putMVar m 'x'
	r <- takeMVar m
	print r
	