--Aufgabe 1

--Aufgabe 1a)

data Tree a = V a | F a [Tree a]

foldTree :: (a -> val) -> (a -> valL -> val) -> valL -> (val -> valL -> valL) -> Tree a -> val
foldTree f g _ _ (V a) = f a
foldTree f g nil h (F a ts) = g a $ foldTrees f g nil h ts

or_ :: Tree Bool -> Bool
or_ a = foldTree (\x -> x) (\

--Aufgabe 1b)

data Bintree a = Empty | Fork a (Bintree a) (Bintree a)

foldBtree :: val -> (a -> val -> val -> val) -> Bintree a -> val
foldBtree val _ Empty = val
foldBtree val f (Fork a left right) = f a (foldBtree val f left) (foldBtree val f right)

preorderB :: Bintree a -> [a]
