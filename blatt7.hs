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
instance Show Nat where
 show Zero = "0" 
 show (Succ x) = (help x y) where
  help Zero b = "x"
  help (Succ x) a = help x (a+1)

--Aufgabe 1e)
instance Num Nat where
 negate = undefined
 abs n = n
 signum Zero = Zero
 signum n = Succ Zero
 fromInteger = toEnum. toInteger
 (+)
 (*)
