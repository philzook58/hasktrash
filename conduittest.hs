




-- Conduit

-- streaming

-- processing sequences

-- not all data in memeory at once

-- laziness
import Conduit


main = print $ runConduitPure $ yieldMany [1..2] .| square' .| (sumC' 0)

--sinkList
-- takeC
-- mapC
-- takeWhileC
-- mapM_C print will put a print in there
-- Mmm. MapM print is a way to print


square :: (Num t, Monad m) => ConduitM t t m ()
square = do
   x <- await
   case x of
     Nothing -> return ()
     Just y -> do yield (y*y)
                  square

square' :: (Num t, Monad m) => ConduitM t t m ()
square' = awaitForever (\x -> yield (x * x))


sumC' acc = do
  x <- await
  case x of
    Nothing -> return acc
    Just s -> sumC' (s + acc)
--example2 = runConduitRes $ sourceFile "input.txt" .| sinkFile "output.txt"


--example3 = runConduitPure $ yieldMany [1..10] .| mapC (* 1) .| sinkList

-- pipeline
-- .| .| .| are the pipes

--
-- foo can yield , it's like python generators

-- await from upstream

-- fuse or .|
-- streams of values
-- () is total input
-- Void is output

-- starts at downstream
-- keep processing until it awaits
-- control passed upstream
-- somebody yields
-- downstream will always get control again
-- upstream may not

-- COnduitM i o m r

-- runcodnuit makes  m r
-- 

