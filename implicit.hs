newtype Modulus s a = Madulus a deriving (Eq, Show)
newtype M s a = M a deriving (Eq,Show)


add :: Integral a => Modulus s a -> M s a -> M s a -> M s a
add (Modulus m) (M a) (M b) = M (mod (a + b) m)


unM :: M s a -> a
unM (M a) = a

