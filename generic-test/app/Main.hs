{-# LANGUAGE DeriveGeneric
, TypeOperators
, FlexibleInstances, TypeFamilies  #-}

module Main where

import Lib

import GHC.Generics


data TestRecord = TestRecord {first :: Int, second :: [Int], third :: String, fourth :: Int} deriving (Generic, Eq, Show)

t1 = TestRecord 3 [2,3,4] "hey" 5

class ExtractFirst f where
	tocs :: f p -> Int

instance ExtractFirst f => ExtractFirst (M1 i t f) where
	tocs (M1 x) = tocs x
instance ExtractFirst f => ExtractFirst (f :*: g) where
	tocs (x :*: y) = tocs x
instance ExtractFirst (K1 i Int) where
	tocs (K1 x) = x

data TestInt = TestInt Int deriving Generic
data NotInt = NotInt String deriving Generic

main :: IO ()
main = do putStrLn  $ show t1
          putStrLn $ show $ tocs $ from t1 
          putStrLn $ show $ isint $ from (TestInt 5)
          putStrLn $ show $ isint $ from (NotInt "no an int")

{-
class RecordInt f where
	tolistin :: f p -> [(String, Int)]

instance ExtractFirst f => ExtractFirst (M1 i t f) where
	tocs (M1 x) = tocs x
instance ExtractFirst f => ExtractFirst (f :*: g) where
	tocs (x :*: y) = tocs x
instance ExtractFirst (K1 i Int) where
	tocs (K1 x) = x
instance ExtractFirst (K1 i a) where
	tocs (K1 x) = []
-}


class IsInt f where
	isint :: f p -> Maybe Int
{-
instance IsInt (IsInt' a) => IsInt (K1 i a) where
    isint (K1 x) = isint
instance IsInt (HTrue) where
	isint (HTrue x) = Just x


instance IsInt (HFalse) where
	isint _ = Nothing
-}
instance IsInt (K1 i Int) where
    isint (K1 x) = Just x
instance {-# OVERLAPS #-} IsInt (K1 i a) where
    isint (K1 x) = Nothing

instance IsInt f => IsInt (M1 i t f) where
	isint (M1 x) = isint x


data HTrue = HTrue Int
data HFalse = HFalse

type family IsInt' a where
  IsInt' Int    = HTrue
  IsInt' _   =  HFalse


{-

data    V1        p                       -- lifted version of Empty
data    U1        p = U1                  -- lifted version of ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper

-}