data TrampolineT m a = Return a | Bounce (m (Trampoline m a))

instance Functor m => Functor (TrampolineT m) where
	fmap f (Return x) = Return (f x)
	fmap f (Bounce y) = (fmap . fmap) f y

