import Control.Monad

--Aufgabe 1
data Point = Point {x,y :: Float} deriving Show

newtype State state a = State {runS :: state -> (a,state)}

p = Point 1 2

--Aufgabe 1a
getX :: State Point Float
getX = State $ \s -> (x s, s) 

getY :: State Point Float
getY = State $ \s -> (y s, s)

--Aufgabe 1b
setX :: Float -> State Point ()
setX x = State $ \s -> ((), s{x=x}) 

setY :: Float -> State Point ()
setY y = State $ \s -> ((), s{y=y}) 

--Aufgabe 1c
{-
rotate :: (Float, Float) -> Float -> State Point ()
rotate (m,n) r = State $ \s -> (runS $ setX $ m+x1 * b - y1*a s >> runS $ setY $ b+x1 * a + y1*b s) where
	a = sin rad
	b = cos rad
	rad = r*pi/180
	x1= State $ \s -> (fst $ runS getX s)
	y1 = State $ \s -> (fst $ runS getY s)
-}
--if (State $ \p -> ((),(x,y)) == State $ \s -> s) then State $ \s -> ((), s) else 

rotateDo :: (Float, Float) -> Float -> State Point ()
rotateDo (i,j) r = do
 let rad = r*pi/180
 let s =  sin rad
 let c = cos rad
 x1 <- getX
 y1 <- getY
 setX(x1 + i)
 setY(y1 + j)
 x1' <- getX
 y1' <- getY
 setX (x1' * c - y1 * s)
 setY (y1' * s + y1 * c)
 
