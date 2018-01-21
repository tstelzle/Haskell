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

--Aufgabe 1d)
instance Show Nat where
 show a = shows (help2 a) "" 

help2 :: Nat -> Int
help2 (Succ b) = 1 + (help2 b)
help2 Zero = 0

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

--Aufgabe 2 NICHT GELOEST
data STree a = BinS (STree a) a (STree a) | LeftS (STree a) a | RightS a (STree a) | LeafS a

{-
instance Show (STree a) where
 show (LeafS a) = shows a ""
 show (LeftS x y) = undefined
 show (RightS x y) = undefined
 show (BinS x y z) = undefined
-}
--Aufgabe 3
data Tree a = V a | F a [Tree a] deriving Show

baum = F True [V True, F True [V True, V False]]
baum2 = F False [V True, V False]

--Aufgabe 3a)
treeAnd :: Tree Bool -> Bool
treeAnd (V a) = if (a == True) then True else False
treeAnd (F a xs) = if (a == False) then False else and $ map treeAnd xs

--Aufgabe 3b)
treeZip :: Tree a -> Tree b -> Tree (a,b)
treeZip (V a) (V b) = (V (a,b))
treeZip (V a) (F b bs) = (V (a,b))
treeZip (F a as) (V b) = (V (a,b))
treeZip (F a as) (F b bs) = (F (a,b) (help as bs))

help :: [Tree a] -> [Tree b] -> [Tree (a,b)]
help [] _ = []
help _ [] = []
help ((V a):as) ((V b):bs) = (V (a,b)) : (help as bs) 
help ((F a ass):as) ((V b):bs) = (V (a,b)) : (help as bs)
help ((V a):as) ((F b bss):bs) = (V (a,b)): (help as bs)
help ((F a ass):as) ((F b bss):bs) = (F (a,b) (help ass bss)) : (help as bs)
