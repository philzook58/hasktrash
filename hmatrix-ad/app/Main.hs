{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib

import Control.Category
import Numeric.LinearAlgebra

type D a =  Matrix a-- Vector a -> Vector a

-- Actually, we can't Make a cetgory without having the input and output spaces as types.

type AD a = (Vector a -> (Vector a, D a))


data BMat a = Base (Matrix a) | VStack (Bmat a) (BMat a) | HStack (BMat a) (BMat a)
data BMatF a f = BaseF a | VStackF f f | HStackF f f

-- As long as I don't need to invert, the function form of Matrices = Vec -> Vec is quite nice.

--? x is evalution point, 1 is for initializing the 
set funlens 1 x 
-- <%~ would evaluated and set to 1? 

-- Kind of Lens-like?
type AD' = (Vector Double) -> (Vector Double, (Vector Double -> Vector Double))
type AD'' a b = (a -> (b, a -> b)) -- forward
type AD'' a b = (a -> (b, b -> a)) -- backward This is a lens? Lens' actually. Curious
-- A function could in some sense be tabulated. So a function is a huge product type. Evaluating the function is getting that value out of it.
-- but a isn't a function... 
-- we'd give it a new value we want it to achieve there... which we could try to do using a gradient step. This is a fitting procedure. Sample function, give new value we actualy want.
-- b is in some sense latent in a. it is the Sine view of a.
-- a differential is in some sense a setter? it does give insturction for how we're going to change the function
-- we're really stretching here
type EvalLens a b = Lens' (a -> b) b
type FunLens a b = (a -> b) -> (b, (b -> (a->b)))
type FunLens a b = a -> (Paramfun w a b) -> (b, (b -> (ParamFun w a b))) -- This lens tells you how to evaluate ParamFuns and how to update them.
type ParamFun w a b = Weights w (AD'' (w,a) b) -- Weights plus autodifferentiable function, we can compose ParamFuns by composiion on AD and (w1,w2) 
-- full stab form
type AD''' a b da db = (a -> (b, db -> da))
data AD''' a b = AD''' (a -> (b, b -> AD''' a b))

comp :: AD' -> AD' -> AD'
comp f g = \x -> let (v, d) = g x in
				 let (v', d') = f v in (v', d' . d)

id' :: AD'
id' = \x -> (x, id)
-- Maybe need additive? Gives us zero

dup :: Number a => AD'' a (a,a)
dup = \x -> ((x,x), uncurry (+)) 


split f g = comp (par f g) dup

add :: Number a => AD'' (a,a) a
add (x,y) = (x + y, \z -> (z,z))

fst :: Additive b => AD'' (a,b) a
fst (x,y) = (x, \z -> (z,zero))

snd :: Additive a => AD'' (a,b) b
snd (x,y) = (y, \z -> (zero,z))

const x = \_ -> (x, const zero)


-- in addition to compositional style, there is also a kind of a monad?
-- But not quite since I'd like to have two type parameters
instance Monad (b, a -> b) where -- Monad (a, a -> a)
	(x, d) >>= f = let (v, d') = f x in (v, d . d')
	return x = (x , id)

-- Replace tupling scturute with lists and make all elemts the same (Vector Double typically)
-- more faithful type flattening would be to a Tree structure rather than a list
data ADMonad a = ADMonad [a] ([a]->[a]) 
-- applicative? And aplicative's relation to pulling tuples inside


mul (x,y) = (x*y, z -> (z * y, x * z))

square = comp mul dup

pow n = iterate n () $

{- REWRITE -}

par :: AD'' a b -> AD'' c d -> AD'' (a,c) (b,d)
par f g = \(x,y) -> let (v, d) = g x in
				    let (v', d') = f y in ((v,v'), d *** d')

instance Category AD'' where
	id = id'
	f . g = comp

max (x,y) = (max x y, \z -> if x < y then (z,0) else (0,z)) 



instance Number a, Number b => Number (AD'')
	(+)
	(*)
	negate
	sign

instance Fractional


vdup x = VStack x x
hdup x = HStack x x

transpose (VStack x y) = HStack (transpose x) (transpose y)






comp :: Numeric a => AD a -> AD a -> AD a
comp f g = \x -> let (v, d) = g x in
				 let (v', d') = f v in (v', d' <> d)
					  

id' :: (Num a, Element a, Container Vector a) => AD a
id' = \x -> (x, ident (size x))


newtype Vector' a b = Vector' (Vector a) (Iso b Int) -- Parametrized by types? And isomorphism to Integer indices
newtype Vector' a b = Vector' (Vector a) -- phantom type style. Could have implicit Iso b Int from enum tpyeclass.
newtype Matrix' a b c = Matrix' (Matrix a) (b -> Int) (Int -> c)

--gadt style
 Base :: Matrix a -> Matrix' a b c -- unsafe? 
 Id :: Matrix' a b b
 HStack
 VStack :: Matrix' a b c -> Matrix' a d c -> Matrix' a (Either a d) c
-- types do not track hstack vstack ordering. which is fine since they do naturally commute
-- So Perhaps should be using a typelevel list for these tags then
-- and vstack and hstack should be implemented in a more natyrual two dimensional structure (Block?)

-- a safe constructor
constructBase :: ((b,c) -> a) -> Matrix' a b c

--instance Number a => Category (AD a) -- Can't Do it. Needs to be a constrained category. Nope. Just a sepcial tag for Id
instance Number a => Category (Matrix' a) where
	id = ident ?
	(.) = <>



-- cps with implied
data CPSMatrix a b c e = CPSMatrix (Matrix a -> e) -> e
data Mat' a b c = MFun (Vector a -> Vector a) | Dense (Matrix a) | Id

data Vec' = Zero | Base Vector a | DSum Vec' Vec' -- A sligthly enhanced Vector type. Good for splitting and possible Zeros

instance Number a => Category (Mat' a) where
	id = MFun id
	id = Id
	Id . x = x
	x . Id = x
	x . y = x <> y
	MFun (.) Dense = MFun \x -> Dot
	Dense . Dense = Dense <>





main :: IO ()
main = someFunc
