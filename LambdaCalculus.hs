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

-------------------------------------
--Part 13: Partial Abstract Machine--
-------------------------------------
state1 = (Lambda "x" (Lambda "y" (Variable "x")) , [Variable "Yes", Variable "No"])
term1 = Apply (Apply (Lambda "x" (Lambda "y" (Variable "x"))) (Variable "Yes")) (Variable "No")
term2 = Apply (Apply (Lambda "b" (Apply example (Variable "Yes"))) (Lambda "z" (Variable "z"))) (Variable "No")

type PState = (Term, [Term])

p_start :: Term -> PState
p_start t = (t, [])

p_step :: PState -> PState
p_step ((Lambda z n), (m:s)) = (substitute z m n, s)
p_step ((Apply n m, ms)) = (n, m:ms)
p_step ((n, m)) = (n, m)

p_final :: PState -> Bool
p_final ((Lambda z n), []) = True
p_final (Variable v, _) = True
p_final _ = False

p_run :: Term -> IO ()
p_run p = do
        let t = p_start p in run_machine t
            where    
                run_machine :: PState -> IO ()
                run_machine t = do
                   if p_final t
                   then do
                       print t
                       print (p_readback t)
                   else do
                       print t
                       run_machine (p_step t)
        
p_readback :: PState -> Term
p_readback ((t, p:ps)) = p_readback ((Apply t p), ps)
p_readback((p, [])) = p

-------------------------------------
--Part 14: Krivine Abstract Machine--
-------------------------------------
data Closure = Closure (Term, Env)
instance Show Closure where show = pretty_closure 

data Env = Env [(Var, Closure)]
instance Show Env where show = pretty_env

data State = State (Closure, [Closure])
instance Show State where show = pretty_state

pretty_closure :: Closure -> String
pretty_closure (Closure (c, Env [])) = "(" ++ show c ++ ",[])"
pretty_closure (Closure c) = show c

pretty_env :: Env -> String
pretty_env (Env []) = ""
pretty_env (Env e) = show e

pretty_state :: State -> String
pretty_state (State (c, [])) = "(" ++ pretty_closure c ++ ",[])"
pretty_state (State (c, stack_c)) = "(" ++ pretty_closure c ++ "," ++ show stack_c ++ ")"

state2 = State ((Closure ((Apply (Lambda "x" (Variable "x")) (Variable "y")), Env [("y", Closure ((Lambda "z" (Variable "z")), (Env [])))])), [])
state3 = State (Closure ((Apply (Variable "x") (Variable "x")), Env [("x", Closure (Lambda "x" (Apply (Variable "x") (Variable "x")), Env []))]), []) 
state4 = State (Closure ((Lambda "y" (Variable "x")), Env []), [Closure ((Variable "z"), Env [("z", Closure ((Lambda "a" (Variable "b")), Env [("b", Closure ((Variable "c"), Env []))]))])])
 
start :: Term -> State
start t = State(Closure(t, Env []), [])

step :: State -> State
step (State (Closure ((Variable x), Env ((y, Closure (n, f)):e)), es)) = if x == y 
                                                                         then State (Closure (n, f), es) 
                                                                         else State (Closure (Variable x, Env e), es)                                                                       
step (State (Closure ((Lambda z n), Env e), Closure (m, f):es)) = State (Closure (n, (Env ((z, Closure(m, f)):e))),es)
step (State (Closure ((Apply n m),  e), es)) = State (Closure (n, e), Closure (m,e):es)
 
final :: State -> Bool
final (State (Closure (Lambda z n, e), [])) = True
final (State (Closure (Variable v, Env []), e)) = True
final _ = False

run :: Term -> IO ()
run p = do
        let t = start p in kam_run_machine t
            where    
                kam_run_machine :: State -> IO ()
                kam_run_machine t = do
                   if final t
                   then do
                       print t
                       print (readback t)
                   else do
                       print t
                       kam_run_machine (step t)            

readback :: State -> Term
readback (State(c, [])) = send_back c
readback (State(c, xs)) = Apply (send_back c) (iterate_closures xs) 
    where
        iterate_closures :: [Closure] -> Term
        iterate_closures ([c]) = send_back c
        iterate_closures (x:xs) =  Apply (send_back x) (iterate_closures xs)
        
send_back :: Closure -> Term
send_back (Closure((Variable x, Env []))) = Variable x
send_back (Closure((Variable x, Env ((y, Closure (n, Env e)) : f)))) = if x == y 
                                                                       then send_back (Closure (n, Env e)) 
                                                                       else send_back (Closure (Variable x, Env f))                                                                      
send_back (Closure((Lambda x n), Env e)) = (Lambda x (send_back((Closure (n, Env ((x, (Closure(Variable x, Env []))): e))))))
send_back (Closure ((Apply n m), Env e)) = Apply (send_back (Closure (n, Env e))) (send_back (Closure (m, Env e)))

