{-# LANGUAGE BangPatterns #-}


--REF:  https://www.well-typed.com/blog/2016/01/efficient-queues/


class Queue q where
  empty :: q a
  head  :: q a -> a
  tail  :: q a -> q a
  snoc  :: q a -> a -> q a



data Queue0 a = Q0 [a]

instance Queue Queue0 where
  empty              = Q0 []
  head (Q0 (x:_ ))   = x
  tail (Q0 (_:xs))   = Q0 xs
  snoc (Q0 xs    ) x = Q0 (xs ++ [x])


data StrictList a = SNil | SCons a !(StrictList a) deriving Show 


-- | Append two strict lists
app :: StrictList a -> StrictList a -> StrictList a
app SNil ys         = ys
app (SCons x xs) ys = SCons x (app xs ys)

-- | Reverse a strict list
rev :: StrictList a -> StrictList a
rev = go SNil
  where
    go :: StrictList a -> StrictList a -> StrictList a
    go acc SNil         = acc
    go acc (SCons x xs) = go (SCons x acc) xs


data Queue2 a = Q2 !Int [a] !Int !(StrictList a) deriving Show

rev' :: StrictList a -> [a]
rev' = go []
  where
    go :: [a] -> StrictList a -> [a]
    go acc SNil         = acc
    go acc (SCons x xs) = go (x:acc) xs


inv2 :: Queue2 a -> Queue2 a
inv2 q@(Q2 f xs r ys)
  | f < r     = Q2 (f+r) (xs ++ rev' ys) 0 SNil
  | otherwise = q

instance Queue Queue2 where
  empty                     = Q2 0 [] 0 SNil
  head (Q2 _ (x:_ ) _ _ )   = x
  tail (Q2 f (_:xs) r ys)   = inv2 $ Q2 (f-1) xs r ys
  snoc (Q2 f xs     r ys) y = inv2 $ Q2 f xs (r+1) (SCons y ys)









