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
