import Control.Monad

--Aufgabe1

type BStore x = x -> Bool
data BExp x = True_ | False_ | BVar x | Or [BExp x] |And [BExp x] | Not (BExp x) | Exp x := Exp x |Exp x :<= Exp x
type Store x = x -> Int
data Exp x = Con Int | Var x | Sum [Exp x] | Prod [Exp x] |Exp x :- Exp x | Int :* Exp x | Exp x :^ Int

exp2store :: Exp x -> Store x -> Int
exp2store x = case x of
    Con i -> return i
    Var x -> ($x)
    Sum es -> do is <- mapM exp2store es; return $ sum is
    Prod es -> do is <- mapM exp2store es; return $ product is
    (e :- e') -> do i <- exp2store e; k <- exp2store e'; return $ i-k
    (i :* e) -> do k <- exp2store e; return $ i*k
    (e :^ i) -> do k <- exp2store e; return $ k^i

bexp2store :: BExp x -> Store x -> BStore x -> Bool
bexp2store bs st = case bs of
    True_ -> return True
    False_ -> return False
    (BVar x) -> ($x)
    (Or bs) -> do is <- mapM (flip bexp2store st) bs; return $ or is
    (And bs) -> do is <- mapM (flip bexp2store st) bs; return $ and is
    (Not b) -> do i <- bexp2store b st; return $ not i
    (e1 := e2) -> return $ exp2store e1 st == (exp2store e2 st)
    (e1 :<= e2) -> return $ exp2store e1 st <= (exp2store e2 st)
    
--Aufgabe2
  
instance Monoid state => Monad ((,) state) where
    return a = (mempty,a)
    (st,a) >>= f = (st `mappend` st',b) where (st',b) = f a
  
  
type ID = Int
type Bank = [(ID,Account)]
data Account = Account { balance :: Int, owner :: Client } deriving Show
data Client = Client{ name :: String, surname :: String, address :: String} deriving Show

own1, own2, own3 :: Client
own1 = Client "Max" "Mustermann" "Musterhausen"
own2 = Client "John" "Doe" "Somewhere"
own3 = Client "Erika" "Mustermann" "Musterhausen"

acc1, acc2, acc3 :: Account
acc1 = Account 100 own1
acc2 = Account 0 own2
acc3 = Account 50 own3

bank :: Bank
bank = [(1,acc1), (2,acc2), (3,acc3)]

credit :: Int -> ID -> Bank -> Bank
credit amount id ls
  = updRel ls id entry{ balance = oldBalance + amount} where
    Just entry = lookup id ls
    oldBalance = balance entry
    
debit :: Int -> ID -> Bank -> Bank
debit amount = credit (-amount)

updRel :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updRel ((a,b):r) c d = if a == c then (a,d):r else (a,b):updRel r c d
updRel _ a b         = [(a,b)]

--Aufgabe 2a
logMsg :: String -> (String, ())
logMsg s = (s,())

--Aufgabe 2b
transferLog :: Int -> ID -> ID -> Bank -> (String,Bank)
transferLog amount id1 id2 ls = do
    let bank = debit amount id1 $ credit amount id2 ls
    logMsg ("Der Betrag " ++ show(amount) ++ " wurde von Konto " ++ show(id1) ++ " auf Konto " ++ show(id2) ++ " übertragen.\n")
    return bank

--Aufgabe 2c)
transaction :: Bank -> (String, Bank)
transaction bs = do
 transferLog 50 1 2 bs
 transferLog 25 1 3 bs
 transferLog 25 2 3 bs
 return bank

--Aufgabe 3
--Aufgabe 3a)
putAccount :: ID -> Account -> State Bank ()
putAccount id acc bs = do

