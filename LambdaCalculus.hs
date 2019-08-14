type Var = String

data Term = -- x | lx.M | MN
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
--  deriving Show

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m
      
-------------------------------------
--Part 1: Church numeral calculator--
-------------------------------------
numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeralChurch (i)))
    where
        numeralChurch 0 = Variable "x"
        numeralChurch i = Apply (Variable "f") (numeralChurch(i-1))
        
-------------------------------------
---Part 2: Variable list generator---
-------------------------------------
variables :: [Var]
variables = [[a] | a <- ['a'..'z']] ++ (alphabetList 1)
    where
        alphabetList :: Int -> [Var]
        alphabetList i = [[a] ++ (show i) | a <- ['a'..'z']] ++ (alphabetList (i+1))
        
-------------------------------------
----Part 3: Variable lists filter----
-------------------------------------
filterVariables :: [Var] -> [Var] -> [Var]
filterVariables listA listB = [x | x <- listA, not (elem x listB)]

-------------------------------------
-----Part 4: Variable generation-----
-------------------------------------
fresh :: [Var] -> Var
fresh list = head (filterVariables variables list)

-------------------------------------
-------Part 5: Unused variable-------
-------------------------------------
used :: Term -> [Var]
used (Variable v) = [v]
used (Lambda x term) = merge [x] (used term)
used (Apply term1 term2) = merge (used term1) (used term2)

-------------------------------------
-------Part 6: Rename variable-------
-------------------------------------
rename :: Var -> Var -> Term -> Term
rename x y (Variable z) = if x == z then (Variable y) else (Variable z)
rename x y (Lambda z n) = if x == z then (Lambda z n) else (Lambda z (rename x y n))
rename x y (Apply  n m) = Apply (rename x y n) (rename x y m)

-------------------------------------
---------Part 7: Substitution--------
-------------------------------------
substitute :: Var -> Term -> Term -> Term
substitute x f (Variable v) = if x == v then f else (Variable v) 
substitute x f (Apply  n m) = Apply (substitute x f n) (substitute x f m)
substitute x f (Lambda z n) = if x == z then (Lambda z n) 
                              else (Lambda ld (substitute x f (rename z (ld) n))) 
                              where ld = fresh(merge [x] (merge (used f) (used n)))
                              
-------------------------------------
--------Part 8: Beta reduction-------
-------------------------------------
beta :: Term -> [Term]
beta (Apply (Lambda z n) m) = (substitute z m n):[Apply (Lambda z nn) m | nn <- (beta n)] ++ [Apply (Lambda z n) nm | nm <- (beta m)]
beta (Apply n m) = [Apply nn m | nn <- (beta n)] ++ [Apply n nm | nm <- beta(m)]
beta (Lambda n m) = [Lambda n beta_m | beta_m <- beta m]
beta (Variable v) = []

-------------------------------------
----Part 9: Normal form reduction----
-------------------------------------
normalize :: Term -> IO ()
normalize (toNormalize) = do 
                                print(toNormalize) 
                                let normalizing = (beta (toNormalize))
                                if (length (normalizing) > 0) 
                                then normalize (head (normalizing)) 
                                else return ()
                                
-------------------------------------
---Part 10: Applicative reduction----
------------------------------------- 
a_beta :: Term -> [Term]
a_beta (Apply (Lambda z n) m) = [Apply (Lambda z nn) m | nn <- (a_beta n)] ++ [Apply (Lambda z n) nm | nm <- (a_beta m)] ++ [(substitute z m n)]
a_beta (Apply n m) = [Apply nn m | nn <- (a_beta n)] ++ [Apply n nm | nm <- a_beta(m)]
a_beta (Lambda n m) = [Lambda n a_beta

-------------------------------------
---Part 11: Applicative normalize----
-------------------------------------
a_normalize :: Term -> IO ()
a_normalize (a_toNormalize) = do 
                                print(a_toNormalize) 
                                let a_normalizing = (a_beta (a_toNormalize))
                                if (length (a_normalizing) > 0) 
                                then a_normalize (head (a_normalizing)) 
                                else return ()
                                
-------------------------------------
-----Part 12: Normalize test cases---
-------------------------------------                               
example1 :: Term
example1 = (Apply (numeral 2) (numeral 2))

example2 :: Term
example2 = (Apply (numeral 2) (numeral 0)) 
