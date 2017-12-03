--Aufgabe 1)
data Nat = Zero | Succ Nat

--Aufgabe 1a)
instance Eq Nat where
 (==) Zero Zero = True
 (==) _ Zero = False
 (==) Zero _ = False
 (==) (Succ x) (Succ y) = (==) x y

 (/=) x y = not $ (==) x y

--Aufgabe 1b)
instance Ord Nat where
 (<=) Zero Zero = True
 (<=) Zero (Succ x) = True
 (<=) (Succ x) (Succ y) = (<=) x y
 (<=) _ Zero = False

--Aufgabe 1c)
instance Enum Nat where
 toEnum 0 = Zero
 toEnum x = Succ $ toEnum (x-1)

 fromEnum Zero = 0
 fromEnum (Succ x) = 1 + fromEnum x

--Aufgabe 1d) NICHT GELOEST
{-
instance Show Nat where
 show Zero = "0" 
 show (Succ x) = (help x y) where
  help Zero b = "x"
  help (Succ x) a = help x (a+1)
-}

--Aufgabe 1e)
instance Num Nat where
 negate = undefined
 abs n = n
 signum Zero = Zero
 signum n = Succ Zero
 fromInteger = toEnum . fromInteger

 (+) Zero x = x
 (+) x Zero = Zero
 (+) (Succ x) y = (+) x (Succ y)

 (*) Zero _ = Zero
 (*) _ Zero = Zero
 (*) x y = save (count y) x y 
 
count :: Nat -> Int
count Zero = 0
count (Succ x) = 1 + count x

save :: Int -> Nat -> Nat -> Nat
--Kann nicht null sein, wird in (*) schon abgefangen
save _ (Succ Zero) y = y 
save x (Succ y) z = save x y (makeNat x z)

makeNat :: Int -> Nat -> Nat
makeNat 1 y = Succ y
makeNat x y = Succ $ makeNat (x-1) y

--Aufgabe 1f)
solutions :: [(Nat, Nat, Nat)]
solutions = [ (toEnum a,toEnum b,toEnum c) | c <- [0..], b <- [0..c^2] , a <- [0..c^2], 2*a^3 + 5*b + 2 == c^2]

--Aufgabe 2
data STree a = BinS (STree a) a (STree a) | LeftS (STree a) a | RightS a (STree a) | LeafS a

{-
instance Show (STree a) where
 show (LeafS a) = a
 show (LeftS x y) = y(show x)
 show (RightS x y) = x(show y)
 show (BinS x y z) = y(show x, show z)
-}

treeAnd :: STree Bool -> Bool
treeAnd (LeafS a) = if(a == True) then True else False
treeAnd (RightS a x) = and a (treeAnd x)
treeAnd (LeftS a x) = and (treeAnd a) x
treeAnd (BinS x y z) = and (treeAnd x) $ and y (treeAnd z) 
