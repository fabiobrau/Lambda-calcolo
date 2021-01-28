-- Definition of lambda terms
data LamTerm = Var String | Abs String LamTerm | App LamTerm LamTerm

-- Printing function
l2s (Var n)     = n
l2s (App e1 e2) = "(" ++ (l2s e1) ++ " " ++ (l2s e2) ++ ")"
l2s (Abs n e)   = "\\" ++ n ++ "." ++ (l2s e)

-- Function that check if a variable is free
is_free x (Var y)     = (x == y)
is_free x (Abs y e1)  = if (x==y) then False else is_free x e1
is_free x (App e1 e2) = is_free x e1 || is_free x e2 

-- Function that check if a variable is bound
is_bound x (Var y)     = False
is_bound x (Abs y e1)  = if (x==y) then True else is_bound x e1
is_bound x (App e1 e2) = is_bound x e1 || is_bound x e2 

-- Function that generate fresh Var
-- Auxiliary function that generate a new variable from an old name
fresh_from_str m y = if not ((is_free y m) || (is_bound y m)) 
                        then y 
                     else
                        fresh_from_str m (y ++ "+")
-- fresh a --> String, return a string the is not free and not bound in a 
fresh m = fresh_from_str m "a"

-- Substitution function:
-- (subs x m n) substitutes the variable x in m with the lambda term n
--
-- Case 1
subs x (Var y) n = if (x == y) then n else (Var y)

-- Case 2
subs x (App m1 m2) n = App (subs x m1 n) (subs x m2 n)

-- Case 3
subs x (Abs y m) n = if (x == y) 
                        then (Abs y m) 
                    else 
                        if not ((is_free x m) && (is_free y n))
                           then Abs y (subs x m n)
                        else 
                            Abs (fresh (App m n)) (subs x (subs y m (Var (fresh (App m n)))) n)
                            
-- Check for Alpha equivalence
alpha_eq (Var x) (Var y) = (x == y)
alpha_eq (App m1 m2) (App n1 n2) = (alpha_eq m1 n1) && (alpha_eq m2 n2)
alpha_eq (Abs x m) (Abs y n) = if (x==y) 
                                  then alpha_eq m n
                               else
                                  (not (is_free y m)) && (not (is_free x n)) && (alpha_eq m (subs y n (Var x)))

-- Spiurious cases
--alpha_eq _ _ = False
