 import Control.Monad
 import Control.Monad.Trans
 import Control.Monad.Trans.Maybe



 
greet :: IO ()
greet = do putStr "what is your name"
           n <- getLine
           purStrLn $" Hello " ++ n

mgreet :: MaybeT IO ()
mgreet = do liftIO $ putStr "what is your name"
            n <- liftIO getLine
            liftIO $ putStrLn $ "Hello " ++ n

